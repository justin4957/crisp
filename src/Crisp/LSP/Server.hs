{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Crisp.LSP.Server
-- Description : Language Server Protocol implementation for Crisp
--
-- Provides LSP server functionality for IDE integration, including:
--
-- * Document synchronization
-- * Diagnostics (errors and warnings)
-- * Hover information (types)
-- * Go to definition
-- * Document symbols
-- * Code completion
-- * Document formatting
--
-- == Usage
--
-- The LSP server can be started with:
--
-- @
-- crisp lsp
-- @
--
-- Or integrated with editors via their LSP client configuration.

module Crisp.LSP.Server
  ( -- * Server
    runLspServer
  , LspConfig(..)
  , defaultLspConfig
    -- * Capabilities
  , ServerCapabilities(..)
  , defaultCapabilities
    -- * Document State
  , DocumentState(..)
  , DefinitionInfo(..)
  , emptyDocumentState
  , updateDocument
    -- * Diagnostics
  , Diagnostic(..)
  , DiagnosticSeverity(..)
  , computeDiagnostics
    -- * Hover
  , HoverResult(..)
  , getHoverInfo
  , formatHoverContent
    -- * Go to Definition
  , LocationResult(..)
  , getDefinition
    -- * Symbols
  , DocumentSymbol(..)
  , SymbolKind(..)
  , getDocumentSymbols
  , symbolKindName
    -- * Completion
  , CompletionItem(..)
  , CompletionKind(..)
  , getCompletions
    -- * Formatting
  , formatDocument
    -- * Log Level
  , LogLevel(..)
  ) where

import Crisp.Parser.Parser (parseModule, ParseError)
import Crisp.Syntax.Surface
import Crisp.Syntax.Span (Span(..), Position(..))
import Crisp.Formatter.Format (formatSource, defaultFormatOptions)
import Crisp.Prelude.Core (fullPreludeContext, PreludeContext(..))
import Crisp.Prelude.Functions (preludeFunctions, PreludeFunction(..), fnName)
import Crisp.Prelude.Types (preludeTypes, PreludeType(..), preludeTypeName, preludeTypeConstrs, PreludeConstructor(..), constrName)
import Crisp.Prelude.Effects (preludeEffects, PreludeEffect(..), effectName, effectOperations, EffectOperation(..), opName)
import Crisp.Core.Term (Kind(..))
import qualified Crisp.Core.Term as C

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe, listToMaybe)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | LSP server configuration
data LspConfig = LspConfig
  { lspConfigLogLevel    :: !LogLevel       -- ^ Logging verbosity
  , lspConfigFormatOnSave :: !Bool          -- ^ Auto-format on save
  , lspConfigMaxDiagnostics :: !Int         -- ^ Max diagnostics per file
  } deriving stock (Eq, Show)

-- | Log level for server messages
data LogLevel = LogError | LogWarn | LogInfo | LogDebug
  deriving stock (Eq, Ord, Show)

-- | Default LSP configuration
defaultLspConfig :: LspConfig
defaultLspConfig = LspConfig
  { lspConfigLogLevel = LogInfo
  , lspConfigFormatOnSave = False
  , lspConfigMaxDiagnostics = 100
  }

--------------------------------------------------------------------------------
-- Server Capabilities
--------------------------------------------------------------------------------

-- | Server capabilities advertised to clients
data ServerCapabilities = ServerCapabilities
  { capTextDocumentSync     :: !Bool  -- ^ Document synchronization
  , capHoverProvider        :: !Bool  -- ^ Hover information
  , capDefinitionProvider   :: !Bool  -- ^ Go to definition
  , capCompletionProvider   :: !Bool  -- ^ Code completion
  , capDocumentSymbolProvider :: !Bool -- ^ Document symbols
  , capFormattingProvider   :: !Bool  -- ^ Document formatting
  , capDiagnosticProvider   :: !Bool  -- ^ Diagnostics
  } deriving stock (Eq, Show)

-- | Default server capabilities
defaultCapabilities :: ServerCapabilities
defaultCapabilities = ServerCapabilities
  { capTextDocumentSync = True
  , capHoverProvider = True
  , capDefinitionProvider = True
  , capCompletionProvider = True
  , capDocumentSymbolProvider = True
  , capFormattingProvider = True
  , capDiagnosticProvider = True
  }

--------------------------------------------------------------------------------
-- Document State
--------------------------------------------------------------------------------

-- | State for a single open document
data DocumentState = DocumentState
  { docUri      :: !Text                    -- ^ Document URI
  , docVersion  :: !Int                     -- ^ Document version
  , docContent  :: !Text                    -- ^ Current content
  , docParsed   :: !(Maybe Module)          -- ^ Parsed AST (if successful)
  , docSymbols  :: ![DefinitionInfo]        -- ^ Defined symbols
  , docDiagnostics :: ![Diagnostic]         -- ^ Current diagnostics
  } deriving stock (Show)

-- | Empty document state
emptyDocumentState :: Text -> DocumentState
emptyDocumentState uri = DocumentState
  { docUri = uri
  , docVersion = 0
  , docContent = ""
  , docParsed = Nothing
  , docSymbols = []
  , docDiagnostics = []
  }

-- | Information about a definition
data DefinitionInfo = DefinitionInfo
  { defName     :: !Text           -- ^ Name of the definition
  , defKind     :: !SymbolKind     -- ^ Kind of symbol
  , defSpan     :: !Span           -- ^ Source location
  , defType     :: !(Maybe Text)   -- ^ Type signature (if available)
  } deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Diagnostics
--------------------------------------------------------------------------------

-- | A diagnostic message
data Diagnostic = Diagnostic
  { diagSeverity :: !DiagnosticSeverity
  , diagSpan     :: !Span
  , diagMessage  :: !Text
  , diagCode     :: !(Maybe Text)
  , diagSource   :: !Text
  } deriving stock (Eq, Show)

-- | Diagnostic severity levels
data DiagnosticSeverity
  = SeverityError
  | SeverityWarning
  | SeverityInfo
  | SeverityHint
  deriving stock (Eq, Ord, Show)

-- | Compute diagnostics for a document
computeDiagnostics :: Text -> Text -> [Diagnostic]
computeDiagnostics uri content =
  case parseModule (T.unpack uri) content of
    Left err -> [parseErrorToDiagnostic uri err]
    Right _mod -> []  -- TODO: Add type checking diagnostics

-- | Convert a parse error to a diagnostic
parseErrorToDiagnostic :: Text -> ParseError -> Diagnostic
parseErrorToDiagnostic uri _err = Diagnostic
  { diagSeverity = SeverityError
  , diagSpan = Span (Position 1 1) (Position 1 1) uri
  , diagMessage = "Parse error"  -- TODO: Extract actual error message
  , diagCode = Just "E001"
  , diagSource = "crisp"
  }

--------------------------------------------------------------------------------
-- Hover
--------------------------------------------------------------------------------

-- | Result of a hover request
data HoverResult = HoverResult
  { hoverContents :: !Text    -- ^ Markdown content
  , hoverRange    :: !(Maybe Span)  -- ^ Range to highlight
  } deriving stock (Eq, Show)

-- | Get hover information at a position
getHoverInfo :: DocumentState -> Position -> Maybe HoverResult
getHoverInfo doc pos = do
  -- Find symbol at position
  info <- findSymbolAt doc pos
  let content = formatHoverContent info
  return $ HoverResult content (Just $ defSpan info)

-- | Format hover content for a definition
formatHoverContent :: DefinitionInfo -> Text
formatHoverContent info =
  let kindStr = symbolKindName (defKind info)
      nameStr = defName info
      typeStr = maybe "" (\t -> " : " <> t) (defType info)
  in "```crisp\n" <> kindStr <> " " <> nameStr <> typeStr <> "\n```"

-- | Find symbol at a given position
findSymbolAt :: DocumentState -> Position -> Maybe DefinitionInfo
findSymbolAt doc pos =
  listToMaybe $ filter (spanContains pos . defSpan) (docSymbols doc)

-- | Check if a position is within a span
spanContains :: Position -> Span -> Bool
spanContains pos span =
  let startOk = posLine (spanStart span) < posLine pos ||
                (posLine (spanStart span) == posLine pos &&
                 posColumn (spanStart span) <= posColumn pos)
      endOk = posLine (spanEnd span) > posLine pos ||
              (posLine (spanEnd span) == posLine pos &&
               posColumn (spanEnd span) >= posColumn pos)
  in startOk && endOk

--------------------------------------------------------------------------------
-- Go to Definition
--------------------------------------------------------------------------------

-- | Result of a go-to-definition request
data LocationResult = LocationResult
  { locUri   :: !Text
  , locRange :: !Span
  } deriving stock (Eq, Show)

-- | Get definition location for symbol at position
getDefinition :: DocumentState -> Position -> Maybe LocationResult
getDefinition doc pos = do
  -- For now, only support within-document definitions
  info <- findSymbolAt doc pos
  return $ LocationResult (docUri doc) (defSpan info)

--------------------------------------------------------------------------------
-- Document Symbols
--------------------------------------------------------------------------------

-- | A symbol in a document
data DocumentSymbol = DocumentSymbol
  { symName     :: !Text
  , symKind     :: !SymbolKind
  , symRange    :: !Span
  , symSelectionRange :: !Span
  , symChildren :: ![DocumentSymbol]
  } deriving stock (Eq, Show)

-- | Symbol kinds
data SymbolKind
  = SkModule
  | SkFunction
  | SkType
  | SkConstructor
  | SkEffect
  | SkHandler
  | SkTrait
  | SkVariable
  | SkConstant
  deriving stock (Eq, Show)

-- | Get human-readable name for symbol kind
symbolKindName :: SymbolKind -> Text
symbolKindName = \case
  SkModule      -> "module"
  SkFunction    -> "fn"
  SkType        -> "type"
  SkConstructor -> "constructor"
  SkEffect      -> "effect"
  SkHandler     -> "handler"
  SkTrait       -> "trait"
  SkVariable    -> "let"
  SkConstant    -> "const"

-- | Get document symbols from parsed module
getDocumentSymbols :: Module -> [DocumentSymbol]
getDocumentSymbols mod =
  let modPath = moduleName mod
      modNameText = T.intercalate "." (modulePathSegments modPath)
      modSym = DocumentSymbol
        { symName = modNameText
        , symKind = SkModule
        , symRange = moduleSpan mod
        , symSelectionRange = moduleSpan mod
        , symChildren = mapMaybe definitionToSymbol (moduleDefinitions mod)
        }
  in [modSym]

-- | Convert a definition to a document symbol
definitionToSymbol :: Definition -> Maybe DocumentSymbol
definitionToSymbol = \case
  DefFn fn -> Just $ DocumentSymbol
    { symName = fnDefName fn
    , symKind = SkFunction
    , symRange = fnDefSpan fn
    , symSelectionRange = fnDefSpan fn
    , symChildren = []
    }
  DefType td -> Just $ DocumentSymbol
    { symName = typeDefName td
    , symKind = SkType
    , symRange = typeDefSpan td
    , symSelectionRange = typeDefSpan td
    , symChildren = map constrToSymbol (typeDefConstructors td)
    }
  DefEffect eff -> Just $ DocumentSymbol
    { symName = effectDefName eff
    , symKind = SkEffect
    , symRange = effectDefSpan eff
    , symSelectionRange = effectDefSpan eff
    , symChildren = []
    }
  DefHandler h -> Just $ DocumentSymbol
    { symName = handlerDefName h
    , symKind = SkHandler
    , symRange = handlerDefSpan h
    , symSelectionRange = handlerDefSpan h
    , symChildren = []
    }
  DefTrait t -> Just $ DocumentSymbol
    { symName = traitDefName t
    , symKind = SkTrait
    , symRange = traitDefSpan t
    , symSelectionRange = traitDefSpan t
    , symChildren = []
    }
  DefImpl _ -> Nothing  -- Implementations don't get their own symbol
  DefExternal ext -> Just $ DocumentSymbol
    { symName = extFnDefName ext
    , symKind = SkFunction
    , symRange = extFnDefSpan ext
    , symSelectionRange = extFnDefSpan ext
    , symChildren = []
    }
  DefTypeAlias alias -> Just $ DocumentSymbol
    { symName = typeAliasName alias
    , symKind = SkType
    , symRange = typeAliasSpan alias
    , symSelectionRange = typeAliasSpan alias
    , symChildren = []
    }

-- | Convert a constructor to a symbol
constrToSymbol :: Constructor -> DocumentSymbol
constrToSymbol c = DocumentSymbol
  { symName = getConstructorName c
  , symKind = SkConstructor
  , symRange = getConstructorSpan c
  , symSelectionRange = getConstructorSpan c
  , symChildren = []
  }

-- | Extract name from a constructor
getConstructorName :: Constructor -> Text
getConstructorName = \case
  SimpleConstructor name _ _ -> name
  GadtConstructor name _ _ -> name
  RecordConstructor name _ _ -> name

-- | Extract span from a constructor
getConstructorSpan :: Constructor -> Span
getConstructorSpan = \case
  SimpleConstructor _ _ span -> span
  GadtConstructor _ _ span -> span
  RecordConstructor _ _ span -> span

--------------------------------------------------------------------------------
-- Completion
--------------------------------------------------------------------------------

-- | A completion item
data CompletionItem = CompletionItem
  { compLabel      :: !Text           -- ^ Display text
  , compKind       :: !CompletionKind -- ^ Kind of completion
  , compDetail     :: !(Maybe Text)   -- ^ Type or detail info
  , compInsertText :: !(Maybe Text)   -- ^ Text to insert
  , compDocumentation :: !(Maybe Text) -- ^ Documentation
  } deriving stock (Eq, Show)

-- | Completion item kinds
data CompletionKind
  = CkFunction
  | CkType
  | CkConstructor
  | CkEffect
  | CkKeyword
  | CkVariable
  | CkSnippet
  deriving stock (Eq, Show)

-- | Get completions at a position
getCompletions :: DocumentState -> Position -> Text -> [CompletionItem]
getCompletions doc _pos prefix =
  let -- Get local symbols
      localSyms = map symbolToCompletion (docSymbols doc)
      -- Get prelude symbols
      preludeSyms = getPreludeCompletions
      -- Get keywords
      keywords = getKeywordCompletions
      -- Filter by prefix
      allCompletions = localSyms ++ preludeSyms ++ keywords
  in filter (matchesPrefix prefix) allCompletions

-- | Check if completion matches prefix
matchesPrefix :: Text -> CompletionItem -> Bool
matchesPrefix prefix item =
  T.null prefix || T.toLower prefix `T.isPrefixOf` T.toLower (compLabel item)

-- | Convert a local symbol to a completion
symbolToCompletion :: DefinitionInfo -> CompletionItem
symbolToCompletion info = CompletionItem
  { compLabel = defName info
  , compKind = symbolKindToCompletionKind (defKind info)
  , compDetail = defType info
  , compInsertText = Nothing
  , compDocumentation = Nothing
  }

-- | Map symbol kind to completion kind
symbolKindToCompletionKind :: SymbolKind -> CompletionKind
symbolKindToCompletionKind = \case
  SkModule      -> CkType
  SkFunction    -> CkFunction
  SkType        -> CkType
  SkConstructor -> CkConstructor
  SkEffect      -> CkEffect
  SkHandler     -> CkFunction
  SkTrait       -> CkType
  SkVariable    -> CkVariable
  SkConstant    -> CkVariable

-- | Get completions from the prelude
getPreludeCompletions :: [CompletionItem]
getPreludeCompletions =
  -- Functions
  [ CompletionItem (fnName f) CkFunction (Just $ formatFnType f) Nothing Nothing
  | f <- preludeFunctions
  ] ++
  -- Types
  [ CompletionItem (preludeTypeName t) CkType Nothing Nothing Nothing
  | t <- preludeTypes
  ] ++
  -- Constructors
  [ CompletionItem (constrName c) CkConstructor Nothing Nothing Nothing
  | t <- preludeTypes
  , c <- preludeTypeConstrs t
  ] ++
  -- Effects
  [ CompletionItem (effectName e) CkEffect Nothing Nothing Nothing
  | e <- preludeEffects
  ]

-- | Format function type for display
formatFnType :: PreludeFunction -> Text
formatFnType fn =
  let params = T.intercalate " -> " $ map (formatType . snd) (fnParams fn)
      ret = formatType (fnReturnType fn)
  in if T.null params then ret else params <> " -> " <> ret

-- | Format a type for display
formatType :: C.Type -> Text
formatType = \case
  C.TyVar name _ -> name
  C.TyCon name [] -> name
  C.TyCon name args -> name <> " " <> T.unwords (map formatTypeAtom args)
  C.TyPi "_" from _ to -> formatTypeAtom from <> " -> " <> formatType to
  C.TyPi name from _ to -> "(" <> name <> ": " <> formatType from <> ") -> " <> formatType to
  C.TyForall name _ body -> "forall " <> name <> ". " <> formatType body
  C.TyLazy inner -> "Lazy " <> formatTypeAtom inner
  other -> T.pack (show other)

-- | Format atomic type (with parens if needed)
formatTypeAtom :: C.Type -> Text
formatTypeAtom t@(C.TyVar _ _) = formatType t
formatTypeAtom t@(C.TyCon _ []) = formatType t
formatTypeAtom t = "(" <> formatType t <> ")"

-- | Get keyword completions
getKeywordCompletions :: [CompletionItem]
getKeywordCompletions =
  [ CompletionItem kw CkKeyword Nothing Nothing Nothing
  | kw <- keywords
  ]
  where
    keywords =
      [ "fn", "type", "effect", "handler", "handle", "with"
      , "match", "if", "then", "else", "let", "in", "do"
      , "perform", "resume", "forall", "where", "trait", "impl"
      , "module", "import", "export", "external", "lazy", "force"
      , "ref", "mut", "linear", "True", "False", "Unit"
      ]

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

-- | Format a document
formatDocument :: Text -> Either Text Text
formatDocument content = formatSource defaultFormatOptions content

--------------------------------------------------------------------------------
-- Server Entry Point
--------------------------------------------------------------------------------

-- | Run the LSP server (placeholder for actual implementation)
--
-- In a full implementation, this would:
-- 1. Set up JSON-RPC communication over stdin/stdout
-- 2. Handle initialize/initialized handshake
-- 3. Process requests (hover, completion, etc.)
-- 4. Send notifications (diagnostics, etc.)
--
-- For now, this is a stub that would be connected to the lsp library.
runLspServer :: LspConfig -> IO ()
runLspServer _config = do
  putStrLn "Crisp Language Server starting..."
  putStrLn "Capabilities:"
  putStrLn "  - Text document synchronization"
  putStrLn "  - Hover (type information)"
  putStrLn "  - Go to definition"
  putStrLn "  - Code completion"
  putStrLn "  - Document symbols"
  putStrLn "  - Document formatting"
  putStrLn "  - Diagnostics"
  putStrLn ""
  putStrLn "Note: Full LSP protocol implementation requires the 'lsp' library."
  putStrLn "This module provides the core functionality that would be wired"
  putStrLn "to the LSP protocol handlers."

--------------------------------------------------------------------------------
-- Helper: Extract symbols from module
--------------------------------------------------------------------------------

-- | Extract definition information from a parsed module
extractDefinitions :: Module -> [DefinitionInfo]
extractDefinitions mod = mapMaybe extractDef (moduleDefinitions mod)
  where
    extractDef :: Definition -> Maybe DefinitionInfo
    extractDef = \case
      DefFn fn -> Just $ DefinitionInfo
        { defName = fnDefName fn
        , defKind = SkFunction
        , defSpan = fnDefSpan fn
        , defType = Nothing  -- TODO: Extract from type annotation
        }
      DefType td -> Just $ DefinitionInfo
        { defName = typeDefName td
        , defKind = SkType
        , defSpan = typeDefSpan td
        , defType = Nothing
        }
      DefEffect eff -> Just $ DefinitionInfo
        { defName = effectDefName eff
        , defKind = SkEffect
        , defSpan = effectDefSpan eff
        , defType = Nothing
        }
      DefHandler h -> Just $ DefinitionInfo
        { defName = handlerDefName h
        , defKind = SkHandler
        , defSpan = handlerDefSpan h
        , defType = Nothing
        }
      DefTrait t -> Just $ DefinitionInfo
        { defName = traitDefName t
        , defKind = SkTrait
        , defSpan = traitDefSpan t
        , defType = Nothing
        }
      DefExternal ext -> Just $ DefinitionInfo
        { defName = extFnDefName ext
        , defKind = SkFunction
        , defSpan = extFnDefSpan ext
        , defType = Nothing
        }
      DefTypeAlias alias -> Just $ DefinitionInfo
        { defName = typeAliasName alias
        , defKind = SkType
        , defSpan = typeAliasSpan alias
        , defType = Nothing
        }
      DefImpl _ -> Nothing

-- | Update document state after content change
updateDocument :: Text -> Int -> Text -> DocumentState
updateDocument uri version content =
  let parsed = either (const Nothing) Just $ parseModule (T.unpack uri) content
      symbols = maybe [] extractDefinitions parsed
      diagnostics = computeDiagnostics uri content
  in DocumentState
    { docUri = uri
    , docVersion = version
    , docContent = content
    , docParsed = parsed
    , docSymbols = symbols
    , docDiagnostics = diagnostics
    }
