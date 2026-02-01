{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Crisp.Doc.Generate
-- Description : Documentation generator for Crisp modules
--
-- Extracts documentation comments from Crisp source files and generates
-- formatted documentation in Markdown or HTML format.
--
-- == Doc Comment Syntax
--
-- Crisp uses @--- |@ to begin documentation comments:
--
-- @
-- --- |
-- --- Module: MyLib.Utils
-- --- Description: Utility functions
-- ---
-- --- This module provides common utilities.
--
-- --- | Add two integers
-- ---
-- --- Examples:
-- ---   add(1, 2) == 3
-- fn add(x: Int, y: Int) -> Int = x + y
-- @
--
-- == Usage
--
-- @
-- import Crisp.Doc.Generate
--
-- main = do
--   result <- generateDocs Markdown "src/"
--   case result of
--     Right docs -> mapM_ writeModuleDoc docs
--     Left err -> putStrLn $ "Error: " ++ show err
-- @

module Crisp.Doc.Generate
  ( -- * Documentation Generation
    generateDocs
  , generateModuleDocs
  , renderModuleDoc
    -- * Output Formats
  , DocFormat(..)
    -- * Types
  , ModuleDoc(..)
  , ItemDoc(..)
  , FunctionDoc(..)
  , TypeDoc(..)
  , EffectDoc(..)
  , ConstructorDoc(..)
  , OperationDoc(..)
    -- * Doc Comment Parsing
  , DocComment(..)
  , parseDocComment
  , extractDocComments
    -- * Rendering
  , renderMarkdown
  , renderHtml
  ) where

import Crisp.Parser.Parser (parseModule)
import Crisp.Syntax.Surface hiding (DocComment)
import Crisp.Syntax.Span (Span(..), Position(..))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (sortOn, intercalate)
import Control.Monad (forM)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, takeBaseName)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Output format for documentation
data DocFormat
  = Markdown  -- ^ GitHub-flavored Markdown
  | HTML      -- ^ Standalone HTML with styling
  deriving stock (Eq, Show)

-- | Documentation for a module
data ModuleDoc = ModuleDoc
  { modDocName        :: !Text           -- ^ Module name (e.g., "Core.Prelude")
  , modDocSummary     :: !(Maybe Text)   -- ^ Brief description
  , modDocDescription :: !(Maybe Text)   -- ^ Full description
  , modDocItems       :: ![ItemDoc]      -- ^ Documented items
  , modDocSourceFile  :: !Text           -- ^ Source file path
  } deriving stock (Eq, Show)

-- | Documentation for an item (function, type, effect, etc.)
data ItemDoc
  = ItemFunction !FunctionDoc
  | ItemType !TypeDoc
  | ItemEffect !EffectDoc
  | ItemExternal !FunctionDoc
  | ItemTypeAlias !TypeDoc
  deriving stock (Eq, Show)

-- | Documentation for a function
data FunctionDoc = FunctionDoc
  { fnDocName       :: !Text             -- ^ Function name
  , fnDocSignature  :: !Text             -- ^ Type signature
  , fnDocSummary    :: !(Maybe Text)     -- ^ Brief description
  , fnDocDescription :: !(Maybe Text)    -- ^ Full description
  , fnDocExamples   :: ![Text]           -- ^ Usage examples
  , fnDocSeeAlso    :: ![Text]           -- ^ Related items
  } deriving stock (Eq, Show)

-- | Documentation for a type
data TypeDoc = TypeDoc
  { tyDocName        :: !Text            -- ^ Type name
  , tyDocParams      :: ![Text]          -- ^ Type parameters
  , tyDocSummary     :: !(Maybe Text)    -- ^ Brief description
  , tyDocDescription :: !(Maybe Text)    -- ^ Full description
  , tyDocConstructors :: ![ConstructorDoc] -- ^ Constructor docs
  , tyDocExamples    :: ![Text]          -- ^ Usage examples
  , tyDocSeeAlso     :: ![Text]          -- ^ Related items
  } deriving stock (Eq, Show)

-- | Documentation for a constructor
data ConstructorDoc = ConstructorDoc
  { conDocName   :: !Text                -- ^ Constructor name
  , conDocFields :: ![Text]              -- ^ Field types or names
  , conDocDescription :: !(Maybe Text)   -- ^ Description
  } deriving stock (Eq, Show)

-- | Documentation for an effect
data EffectDoc = EffectDoc
  { effDocName        :: !Text           -- ^ Effect name
  , effDocParams      :: ![Text]         -- ^ Type parameters
  , effDocSummary     :: !(Maybe Text)   -- ^ Brief description
  , effDocDescription :: !(Maybe Text)   -- ^ Full description
  , effDocOperations  :: ![OperationDoc] -- ^ Operation docs
  , effDocExamples    :: ![Text]         -- ^ Usage examples
  , effDocSeeAlso     :: ![Text]         -- ^ Related items
  } deriving stock (Eq, Show)

-- | Documentation for an effect operation
data OperationDoc = OperationDoc
  { opDocName      :: !Text              -- ^ Operation name
  , opDocSignature :: !Text              -- ^ Type signature
  , opDocDescription :: !(Maybe Text)    -- ^ Description
  } deriving stock (Eq, Show)

-- | A parsed documentation comment
data DocComment = DocComment
  { docSummary     :: !(Maybe Text)      -- ^ First line (brief)
  , docDescription :: !(Maybe Text)      -- ^ Remaining paragraphs
  , docExamples    :: ![Text]            -- ^ Example code blocks
  , docSeeAlso     :: ![Text]            -- ^ Cross-references
  , docSince       :: !(Maybe Text)      -- ^ Version introduced
  } deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Documentation Generation
--------------------------------------------------------------------------------

-- | Generate documentation for all Crisp files in a directory
generateDocs :: DocFormat -> FilePath -> IO (Either Text [ModuleDoc])
generateDocs format dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return $ Left $ "Directory not found: " <> T.pack dir
    else do
      files <- findCrispFiles dir
      results <- forM files $ \file -> do
        content <- TIO.readFile file
        return $ generateModuleDocs format (T.pack file) content
      let (errors, docs) = partitionResults results
      if null docs && not (null errors)
        then return $ Left $ T.intercalate "\n" errors
        else return $ Right docs

-- | Generate documentation for a single module
generateModuleDocs :: DocFormat -> Text -> Text -> Either Text ModuleDoc
generateModuleDocs _format sourcePath content = do
  case parseModule (T.unpack sourcePath) content of
    Left _err -> Left $ "Parse error in " <> sourcePath
    Right mod' -> Right $ extractModuleDoc sourcePath content mod'

-- | Extract documentation from a parsed module
extractModuleDoc :: Text -> Text -> Module -> ModuleDoc
extractModuleDoc sourcePath content mod' =
  let modPath = moduleName mod'
      modName = T.intercalate "." (modulePathSegments modPath)
      docComments = extractDocComments content
      moduleComment = case moduleDocComment mod' of
        Just astDoc -> Just (parseAstDocComment astDoc)
        Nothing     -> let moduleStartLine = posLine (spanStart (moduleSpan mod'))
                       in lookupModuleComment moduleStartLine docComments
      items = mapMaybe (extractItemDoc docComments) (moduleDefinitions mod')
  in ModuleDoc
    { modDocName = modName
    , modDocSummary = docSummary =<< moduleComment
    , modDocDescription = docDescription =<< moduleComment
    , modDocItems = items
    , modDocSourceFile = sourcePath
    }

-- | Extract documentation for a definition
extractItemDoc :: Map Int DocComment -> Definition -> Maybe ItemDoc
extractItemDoc docs def = case def of
  DefFn fn ->
    let line = posLine (spanStart (fnDefSpan fn))
        doc = Map.lookup line docs
    in Just $ ItemFunction $ FunctionDoc
      { fnDocName = fnDefName fn
      , fnDocSignature = formatFnSignature fn
      , fnDocSummary = doc >>= docSummary
      , fnDocDescription = doc >>= docDescription
      , fnDocExamples = maybe [] docExamples doc
      , fnDocSeeAlso = maybe [] docSeeAlso doc
      }

  DefType td ->
    let line = posLine (spanStart (typeDefSpan td))
        doc = Map.lookup line docs
    in Just $ ItemType $ TypeDoc
      { tyDocName = typeDefName td
      , tyDocParams = map formatTypeParam (typeDefParams td)
      , tyDocSummary = doc >>= docSummary
      , tyDocDescription = doc >>= docDescription
      , tyDocConstructors = map extractConstructorDoc (typeDefConstructors td)
      , tyDocExamples = maybe [] docExamples doc
      , tyDocSeeAlso = maybe [] docSeeAlso doc
      }

  DefEffect eff ->
    let line = posLine (spanStart (effectDefSpan eff))
        doc = Map.lookup line docs
    in Just $ ItemEffect $ EffectDoc
      { effDocName = effectDefName eff
      , effDocParams = []  -- Effects don't have params in current Surface AST
      , effDocSummary = doc >>= docSummary
      , effDocDescription = doc >>= docDescription
      , effDocOperations = map extractOperationDoc (effectDefOperations eff)
      , effDocExamples = maybe [] docExamples doc
      , effDocSeeAlso = maybe [] docSeeAlso doc
      }

  DefExternal ext ->
    let line = posLine (spanStart (extFnDefSpan ext))
        doc = Map.lookup line docs
    in Just $ ItemExternal $ FunctionDoc
      { fnDocName = extFnDefName ext
      , fnDocSignature = formatExternalSignature ext
      , fnDocSummary = doc >>= docSummary
      , fnDocDescription = doc >>= docDescription
      , fnDocExamples = maybe [] docExamples doc
      , fnDocSeeAlso = maybe [] docSeeAlso doc
      }

  DefTypeAlias alias ->
    let line = posLine (spanStart (typeAliasSpan alias))
        doc = Map.lookup line docs
    in Just $ ItemTypeAlias $ TypeDoc
      { tyDocName = typeAliasName alias
      , tyDocParams = map formatTypeParam (typeAliasParams alias)
      , tyDocSummary = doc >>= docSummary
      , tyDocDescription = doc >>= docDescription
      , tyDocConstructors = []
      , tyDocExamples = maybe [] docExamples doc
      , tyDocSeeAlso = maybe [] docSeeAlso doc
      }

  _ -> Nothing

-- | Extract constructor documentation
extractConstructorDoc :: Constructor -> ConstructorDoc
extractConstructorDoc = \case
  SimpleConstructor name types _ -> ConstructorDoc
    { conDocName = name
    , conDocFields = map formatSurfaceType types
    , conDocDescription = Nothing
    }
  GadtConstructor name ty _ -> ConstructorDoc
    { conDocName = name
    , conDocFields = [formatSurfaceType ty]
    , conDocDescription = Nothing
    }
  RecordConstructor name fields _ -> ConstructorDoc
    { conDocName = name
    , conDocFields = map formatField fields
    , conDocDescription = Nothing
    }

-- | Extract operation documentation
extractOperationDoc :: Operation -> OperationDoc
extractOperationDoc op = OperationDoc
  { opDocName = operationName op
  , opDocSignature = formatOperationSignature op
  , opDocDescription = Nothing
  }

--------------------------------------------------------------------------------
-- Doc Comment Parsing
--------------------------------------------------------------------------------

-- | Extract doc comments from source text
-- Returns a map from line number to doc comment
extractDocComments :: Text -> Map Int DocComment
extractDocComments content =
  let ls = zip [1..] (T.lines content)
      docBlocks = findDocBlocks ls
  in Map.fromList [(endLine, parseDocComment block) | (endLine, block) <- docBlocks]

-- | Find contiguous blocks of doc comments
findDocBlocks :: [(Int, Text)] -> [(Int, [Text])]
findDocBlocks [] = []
findDocBlocks ((lineNum, line):rest)
  | isDocLine line =
      let (block, remaining) = collectDocBlock ((lineNum, line):rest)
          endLine = lineNum + length block - 1
      in (endLine + 1, map snd block) : findDocBlocks remaining
  | otherwise = findDocBlocks rest

-- | Collect a contiguous doc comment block
collectDocBlock :: [(Int, Text)] -> ([(Int, Text)], [(Int, Text)])
collectDocBlock [] = ([], [])
collectDocBlock ((n, l):rest)
  | isDocLine l =
      let (more, remaining) = collectDocBlock rest
      in ((n, l):more, remaining)
  | otherwise = ([], (n, l):rest)

-- | Check if a line is a doc comment
isDocLine :: Text -> Bool
isDocLine line =
  let stripped = T.stripStart line
  in T.isPrefixOf "--- " stripped || stripped == "---"

-- | Parse a doc comment block from raw source lines
parseDocComment :: [Text] -> DocComment
parseDocComment ls =
  let cleaned = map stripDocPrefix ls
      (summary, rest) = extractSummary cleaned
      (description, examples, seeAlso, since) = parseDocSections rest
  in DocComment
    { docSummary = summary
    , docDescription = description
    , docExamples = examples
    , docSeeAlso = seeAlso
    , docSince = since
    }

-- | Parse a doc comment from already-stripped AST text.
-- The AST stores doc comments as plain text with prefixes removed,
-- so we split by lines and extract sections directly.
parseAstDocComment :: Text -> DocComment
parseAstDocComment text =
  let linesList = T.lines text
      (summary, rest) = case linesList of
        []     -> (Nothing, [])
        (l:ls) -> let stripped = T.strip l
                  in if T.null stripped
                     then case ls of
                       []      -> (Nothing, [])
                       (l2:r2) -> (Just (T.strip l2), r2)
                     else (Just stripped, ls)
      (description, examples, seeAlso, since) = parseDocSections rest
  in DocComment
    { docSummary = summary
    , docDescription = description
    , docExamples = examples
    , docSeeAlso = seeAlso
    , docSince = since
    }

-- | Strip the doc comment prefix from a line.
-- Removes the @--- @ prefix and then the optional @| @ pipe marker,
-- so both @--- | text@ and @--- text@ produce @text@.
stripDocPrefix :: Text -> Text
stripDocPrefix line =
  let stripped = T.stripStart line
      content = if T.isPrefixOf "--- " stripped
                then T.drop 4 stripped
                else if stripped == "---"
                then ""
                else stripped
  in stripPipePrefix content

-- | Strip the optional pipe prefix from a doc comment line.
-- Handles @| text@ (pipe + space + text) and lone @|@.
stripPipePrefix :: Text -> Text
stripPipePrefix t
  | T.isPrefixOf "| " t = T.drop 2 t
  | t == "|"             = ""
  | otherwise             = t

-- | Extract the summary (first paragraph)
extractSummary :: [Text] -> (Maybe Text, [Text])
extractSummary [] = (Nothing, [])
extractSummary (l:ls)
  | T.isPrefixOf "|" l =
      -- Doc comment starts with "--- |"
      let summaryLine = T.strip (T.drop 1 l)
      in if T.null summaryLine
         then extractSummary ls  -- Empty first line, continue
         else (Just summaryLine, ls)
  | T.null (T.strip l) = extractSummary ls
  | otherwise = (Just (T.strip l), ls)

-- | Parse doc sections (description, examples, etc.)
parseDocSections :: [Text] -> (Maybe Text, [Text], [Text], Maybe Text)
parseDocSections ls =
  let (descLines, rest) = span (not . isSectionHeader) ls
      description = if null descLines
                    then Nothing
                    else Just $ T.strip $ T.unlines descLines
      examples = extractSection "Examples:" rest
      seeAlso = extractSection "See also:" rest
      since = listToMaybe $ extractSection "Since:" rest
  in (description, examples, seeAlso, since)

-- | Check if a line is a section header
isSectionHeader :: Text -> Bool
isSectionHeader line =
  any (`T.isPrefixOf` line) ["Examples:", "See also:", "Since:", "Note:", "Warning:"]

-- | Extract content under a section header
extractSection :: Text -> [Text] -> [Text]
extractSection header ls =
  case dropWhile (not . T.isPrefixOf header) ls of
    [] -> []
    (_:rest) ->
      let content = takeWhile (\l -> T.null l || T.isPrefixOf "  " l) rest
      in map T.strip content

-- | Look up the module-level doc comment
-- Only matches doc comments that appear before the module declaration line,
-- so definition doc comments aren't incorrectly stolen as module descriptions.
lookupModuleComment :: Int -> Map Int DocComment -> Maybe DocComment
lookupModuleComment moduleStartLine docs =
  -- A doc comment stored at key K means it ends just before line K.
  -- Only consider comments whose target line is before the module declaration.
  listToMaybe $ Map.elems $ Map.filterWithKey (\k _ -> k < moduleStartLine) docs

--------------------------------------------------------------------------------
-- Rendering - Markdown
--------------------------------------------------------------------------------

-- | Render a module doc to its output format
renderModuleDoc :: DocFormat -> ModuleDoc -> Text
renderModuleDoc Markdown = renderMarkdown
renderModuleDoc HTML = renderHtml

-- | Render module documentation as Markdown
renderMarkdown :: ModuleDoc -> Text
renderMarkdown ModuleDoc{..} = T.unlines $
  [ "# " <> modDocName
  , ""
  ] ++
  maybe [] (\s -> [s, ""]) modDocSummary ++
  maybe [] (\d -> [d, ""]) modDocDescription ++
  [ "## Contents"
  , ""
  ] ++
  renderTableOfContents modDocItems ++
  [ ""
  , "---"
  , ""
  ] ++
  concatMap renderItemMarkdown modDocItems ++
  [ ""
  , "---"
  , ""
  , "*Generated from `" <> modDocSourceFile <> "`*"
  ]

-- | Render table of contents
renderTableOfContents :: [ItemDoc] -> [Text]
renderTableOfContents items =
  concatMap renderTocEntry items

-- | Render a TOC entry
renderTocEntry :: ItemDoc -> [Text]
renderTocEntry item = case item of
  ItemFunction fn -> ["- [`" <> fnDocName fn <> "`](#" <> slugify (fnDocName fn) <> ")"]
  ItemType ty -> ["- [`" <> tyDocName ty <> "`](#" <> slugify (tyDocName ty) <> ")"]
  ItemEffect eff -> ["- [`" <> effDocName eff <> "`](#" <> slugify (effDocName eff) <> ")"]
  ItemExternal fn -> ["- [`" <> fnDocName fn <> "`](#" <> slugify (fnDocName fn) <> ") (external)"]
  ItemTypeAlias ty -> ["- [`" <> tyDocName ty <> "`](#" <> slugify (tyDocName ty) <> ") (alias)"]

-- | Render an item as Markdown
renderItemMarkdown :: ItemDoc -> [Text]
renderItemMarkdown item = case item of
  ItemFunction fn -> renderFunctionMarkdown fn
  ItemType ty -> renderTypeMarkdown ty
  ItemEffect eff -> renderEffectMarkdown eff
  ItemExternal fn -> renderFunctionMarkdown fn
  ItemTypeAlias ty -> renderTypeMarkdown ty

-- | Render function documentation as Markdown
renderFunctionMarkdown :: FunctionDoc -> [Text]
renderFunctionMarkdown FunctionDoc{..} =
  [ "### `" <> fnDocName <> "`"
  , ""
  , "```crisp"
  , fnDocSignature
  , "```"
  , ""
  ] ++
  maybe [] (\s -> [s, ""]) fnDocSummary ++
  maybe [] (\d -> [d, ""]) fnDocDescription ++
  (if null fnDocExamples then [] else
    [ "**Examples:**"
    , ""
    , "```crisp"
    ] ++ fnDocExamples ++
    [ "```"
    , ""
    ]) ++
  (if null fnDocSeeAlso then [] else
    ["**See also:** " <> T.intercalate ", " (map (\x -> "`" <> x <> "`") fnDocSeeAlso), ""])

-- | Render type documentation as Markdown
renderTypeMarkdown :: TypeDoc -> [Text]
renderTypeMarkdown TypeDoc{..} =
  [ "### `" <> tyDocName <> formatParams tyDocParams <> "`"
  , ""
  ] ++
  maybe [] (\s -> [s, ""]) tyDocSummary ++
  maybe [] (\d -> [d, ""]) tyDocDescription ++
  (if null tyDocConstructors then [] else
    [ "**Constructors:**"
    , ""
    ] ++ concatMap renderConstructorMarkdown tyDocConstructors) ++
  (if null tyDocExamples then [] else
    [ "**Examples:**"
    , ""
    , "```crisp"
    ] ++ tyDocExamples ++
    [ "```"
    , ""
    ]) ++
  (if null tyDocSeeAlso then [] else
    ["**See also:** " <> T.intercalate ", " (map (\x -> "`" <> x <> "`") tyDocSeeAlso), ""])

-- | Render constructor documentation as Markdown
renderConstructorMarkdown :: ConstructorDoc -> [Text]
renderConstructorMarkdown ConstructorDoc{..} =
  let fields = if null conDocFields then "" else " " <> T.intercalate " " conDocFields
  in ["- `" <> conDocName <> fields <> "`" <>
      maybe "" (\d -> " - " <> d) conDocDescription]

-- | Render effect documentation as Markdown
renderEffectMarkdown :: EffectDoc -> [Text]
renderEffectMarkdown EffectDoc{..} =
  [ "### `effect " <> effDocName <> formatParams effDocParams <> "`"
  , ""
  ] ++
  maybe [] (\s -> [s, ""]) effDocSummary ++
  maybe [] (\d -> [d, ""]) effDocDescription ++
  (if null effDocOperations then [] else
    [ "**Operations:**"
    , ""
    ] ++ concatMap renderOperationMarkdown effDocOperations) ++
  (if null effDocExamples then [] else
    [ "**Examples:**"
    , ""
    , "```crisp"
    ] ++ effDocExamples ++
    [ "```"
    , ""
    ]) ++
  (if null effDocSeeAlso then [] else
    ["**See also:** " <> T.intercalate ", " (map (\x -> "`" <> x <> "`") effDocSeeAlso), ""])

-- | Render operation documentation as Markdown
renderOperationMarkdown :: OperationDoc -> [Text]
renderOperationMarkdown OperationDoc{..} =
  ["- `" <> opDocName <> "`: `" <> opDocSignature <> "`" <>
   maybe "" (\d -> " - " <> d) opDocDescription]

-- | Format type parameters
formatParams :: [Text] -> Text
formatParams [] = ""
formatParams ps = " " <> T.unwords ps

--------------------------------------------------------------------------------
-- Rendering - HTML
--------------------------------------------------------------------------------

-- | Render module documentation as HTML
renderHtml :: ModuleDoc -> Text
renderHtml ModuleDoc{..} = T.unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"UTF-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "  <title>" <> modDocName <> " - Crisp Documentation</title>"
  , "  <style>"
  , htmlStyles
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <div class=\"container\">"
  , "    <header>"
  , "      <h1>" <> escapeHtml modDocName <> "</h1>"
  , maybe "" (\s -> "      <p class=\"summary\">" <> escapeHtml s <> "</p>") modDocSummary
  , "    </header>"
  , ""
  , "    <nav>"
  , "      <h2>Contents</h2>"
  , "      <ul>"
  , T.unlines $ map renderTocEntryHtml modDocItems
  , "      </ul>"
  , "    </nav>"
  , ""
  , "    <main>"
  , T.unlines $ map renderItemHtml modDocItems
  , "    </main>"
  , ""
  , "    <footer>"
  , "      <p>Generated from <code>" <> escapeHtml modDocSourceFile <> "</code></p>"
  , "    </footer>"
  , "  </div>"
  , "</body>"
  , "</html>"
  ]

-- | CSS styles for HTML output
htmlStyles :: Text
htmlStyles = T.unlines
  [ "    :root { --primary: #2563eb; --bg: #f8fafc; --text: #1e293b; }"
  , "    body { font-family: system-ui, sans-serif; line-height: 1.6; color: var(--text); background: var(--bg); margin: 0; padding: 20px; }"
  , "    .container { max-width: 900px; margin: 0 auto; }"
  , "    header { border-bottom: 2px solid var(--primary); padding-bottom: 1rem; margin-bottom: 2rem; }"
  , "    h1 { color: var(--primary); margin: 0; }"
  , "    .summary { font-size: 1.2rem; color: #64748b; margin-top: 0.5rem; }"
  , "    nav { background: white; padding: 1rem; border-radius: 8px; margin-bottom: 2rem; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }"
  , "    nav h2 { margin-top: 0; font-size: 1rem; color: #64748b; }"
  , "    nav ul { margin: 0; padding-left: 1.5rem; }"
  , "    nav a { color: var(--primary); text-decoration: none; }"
  , "    nav a:hover { text-decoration: underline; }"
  , "    .item { background: white; padding: 1.5rem; border-radius: 8px; margin-bottom: 1rem; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }"
  , "    .item h3 { margin-top: 0; color: var(--primary); }"
  , "    code { background: #e2e8f0; padding: 0.2em 0.4em; border-radius: 4px; font-size: 0.9em; }"
  , "    pre { background: #1e293b; color: #e2e8f0; padding: 1rem; border-radius: 8px; overflow-x: auto; }"
  , "    pre code { background: none; padding: 0; }"
  , "    .constructors, .operations { margin-top: 1rem; }"
  , "    .constructors h4, .operations h4 { font-size: 0.9rem; color: #64748b; margin-bottom: 0.5rem; }"
  , "    footer { text-align: center; color: #64748b; margin-top: 2rem; padding-top: 1rem; border-top: 1px solid #e2e8f0; }"
  ]

-- | Render TOC entry as HTML
renderTocEntryHtml :: ItemDoc -> Text
renderTocEntryHtml item = case item of
  ItemFunction fn -> "        <li><a href=\"#" <> slugify (fnDocName fn) <> "\"><code>" <> escapeHtml (fnDocName fn) <> "</code></a></li>"
  ItemType ty -> "        <li><a href=\"#" <> slugify (tyDocName ty) <> "\"><code>" <> escapeHtml (tyDocName ty) <> "</code></a></li>"
  ItemEffect eff -> "        <li><a href=\"#" <> slugify (effDocName eff) <> "\"><code>" <> escapeHtml (effDocName eff) <> "</code></a></li>"
  ItemExternal fn -> "        <li><a href=\"#" <> slugify (fnDocName fn) <> "\"><code>" <> escapeHtml (fnDocName fn) <> "</code> (external)</a></li>"
  ItemTypeAlias ty -> "        <li><a href=\"#" <> slugify (tyDocName ty) <> "\"><code>" <> escapeHtml (tyDocName ty) <> "</code> (alias)</a></li>"

-- | Render item as HTML
renderItemHtml :: ItemDoc -> Text
renderItemHtml item = case item of
  ItemFunction fn -> renderFunctionHtml fn
  ItemType ty -> renderTypeHtml ty
  ItemEffect eff -> renderEffectHtml eff
  ItemExternal fn -> renderFunctionHtml fn
  ItemTypeAlias ty -> renderTypeHtml ty

-- | Render function as HTML
renderFunctionHtml :: FunctionDoc -> Text
renderFunctionHtml FunctionDoc{..} = T.unlines
  [ "      <div class=\"item\" id=\"" <> slugify fnDocName <> "\">"
  , "        <h3><code>" <> escapeHtml fnDocName <> "</code></h3>"
  , "        <pre><code>" <> escapeHtml fnDocSignature <> "</code></pre>"
  , maybe "" (\s -> "        <p>" <> escapeHtml s <> "</p>") fnDocSummary
  , maybe "" (\d -> "        <p>" <> escapeHtml d <> "</p>") fnDocDescription
  , if null fnDocExamples then "" else
      "        <h4>Examples</h4>\n        <pre><code>" <>
      escapeHtml (T.unlines fnDocExamples) <> "</code></pre>"
  , "      </div>"
  ]

-- | Render type as HTML
renderTypeHtml :: TypeDoc -> Text
renderTypeHtml TypeDoc{..} = T.unlines
  [ "      <div class=\"item\" id=\"" <> slugify tyDocName <> "\">"
  , "        <h3><code>type " <> escapeHtml tyDocName <> escapeHtml (formatParams tyDocParams) <> "</code></h3>"
  , maybe "" (\s -> "        <p>" <> escapeHtml s <> "</p>") tyDocSummary
  , maybe "" (\d -> "        <p>" <> escapeHtml d <> "</p>") tyDocDescription
  , if null tyDocConstructors then "" else
      "        <div class=\"constructors\">\n          <h4>Constructors</h4>\n          <ul>" <>
      T.concat (map (\c -> "\n            <li><code>" <> escapeHtml (conDocName c) <>
                          (if null (conDocFields c) then "" else " " <> escapeHtml (T.unwords (conDocFields c))) <>
                          "</code></li>") tyDocConstructors) <>
      "\n          </ul>\n        </div>"
  , if null tyDocExamples then "" else
      "        <h4>Examples</h4>\n        <pre><code>" <>
      escapeHtml (T.unlines tyDocExamples) <> "</code></pre>"
  , "      </div>"
  ]

-- | Render effect as HTML
renderEffectHtml :: EffectDoc -> Text
renderEffectHtml EffectDoc{..} = T.unlines
  [ "      <div class=\"item\" id=\"" <> slugify effDocName <> "\">"
  , "        <h3><code>effect " <> escapeHtml effDocName <> escapeHtml (formatParams effDocParams) <> "</code></h3>"
  , maybe "" (\s -> "        <p>" <> escapeHtml s <> "</p>") effDocSummary
  , maybe "" (\d -> "        <p>" <> escapeHtml d <> "</p>") effDocDescription
  , if null effDocOperations then "" else
      "        <div class=\"operations\">\n          <h4>Operations</h4>\n          <ul>" <>
      T.concat (map (\o -> "\n            <li><code>" <> escapeHtml (opDocName o) <>
                          "</code>: <code>" <> escapeHtml (opDocSignature o) <> "</code></li>") effDocOperations) <>
      "\n          </ul>\n        </div>"
  , if null effDocExamples then "" else
      "        <h4>Examples</h4>\n        <pre><code>" <>
      escapeHtml (T.unlines effDocExamples) <> "</code></pre>"
  , "      </div>"
  ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Find all .crisp files in a directory (recursively)
findCrispFiles :: FilePath -> IO [FilePath]
findCrispFiles dir = do
  entries <- listDirectory dir
  paths <- forM entries $ \entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isDir
      then findCrispFiles path
      else if isFile && takeExtension path == ".crisp"
           then return [path]
           else return []
  return $ concat paths

-- | Partition results into errors and successes
partitionResults :: [Either Text ModuleDoc] -> ([Text], [ModuleDoc])
partitionResults = foldr go ([], [])
  where
    go (Left e) (es, ds) = (e:es, ds)
    go (Right d) (es, ds) = (es, d:ds)

-- | Format a function signature
formatFnSignature :: FunctionDef -> Text
formatFnSignature fn =
  let params = T.intercalate ", " $ map formatParam (fnDefParams fn)
      ret = maybe "Unit" formatSurfaceType (fnDefReturnType fn)
  in "fn " <> fnDefName fn <> "(" <> params <> ") -> " <> ret

-- | Format an external function signature
formatExternalSignature :: ExternalFnDef -> Text
formatExternalSignature ext =
  let params = T.intercalate ", " $ map formatParam (extFnDefParams ext)
      ret = formatSurfaceType (extFnDefReturnType ext)
      ref = extFnDefExternal ext
  in "external fn " <> extFnDefName ext <> "(" <> params <> ") -> " <> ret <>
     " = (\"" <> externalModule ref <> "\", \"" <> externalFunction ref <> "\")"

-- | Format a parameter
formatParam :: Param -> Text
formatParam p = paramName p <> ": " <> formatSurfaceType (paramType p)

-- | Format a type parameter
formatTypeParam :: TypeParam -> Text
formatTypeParam = \case
  TypeVar name _ _ -> name
  DepParam name ty _ -> "(" <> name <> ": " <> formatSurfaceType ty <> ")"
  BoundedTypeVar name _ constraints _ ->
    "(" <> name <> ": " <> T.intercalate " + " constraints <> ")"

-- | Format a surface type
formatSurfaceType :: Type -> Text
formatSurfaceType = \case
  TyName name _ -> name
  TyApp t1 args _ ->
    formatSurfaceType t1 <> " " <> T.unwords (map formatSurfaceTypeAtom args)
  TyFn from to _ _ -> formatSurfaceTypeAtom from <> " -> " <> formatSurfaceType to
  TyDepFn name from to _ _ -> "(" <> name <> ": " <> formatSurfaceType from <> ") -> " <> formatSurfaceType to
  TyForall tyParam body _ ->
    "forall " <> formatTypeParam tyParam <> ". " <> formatSurfaceType body
  TyLazy inner _ -> "Lazy " <> formatSurfaceTypeAtom inner
  TyRef inner isMut _ -> (if isMut then "Ref mut " else "Ref ") <> formatSurfaceTypeAtom inner
  TyParen inner _ -> "(" <> formatSurfaceType inner <> ")"
  TyRefinement base _ _ -> formatSurfaceType base
  TyHole _ -> "_"
  TyTuple elems _ -> "(" <> T.intercalate ", " (map formatSurfaceType elems) <> ")"

-- | Format atomic surface type
formatSurfaceTypeAtom :: Type -> Text
formatSurfaceTypeAtom t@(TyName _ _) = formatSurfaceType t
formatSurfaceTypeAtom t = "(" <> formatSurfaceType t <> ")"

-- | Format a field
formatField :: Field -> Text
formatField f = fieldName f <> ": " <> formatSurfaceType (fieldType f)

-- | Format operation signature
formatOperationSignature :: Operation -> Text
formatOperationSignature op = formatSurfaceType (operationSignature op)

-- | Create a URL-safe slug from text
slugify :: Text -> Text
slugify = T.toLower . T.replace " " "-" . T.filter isSlugChar
  where isSlugChar c = c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '-', '_'])

-- | Escape HTML special characters
escapeHtml :: Text -> Text
escapeHtml = T.concatMap escape
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape '"' = "&quot;"
    escape c = T.singleton c

-- | Maybe to list
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x
