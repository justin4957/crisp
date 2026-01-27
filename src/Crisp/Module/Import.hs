{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Module.Import
-- Description : Import parsing and resolution
--
-- Functions for parsing import declarations and resolving names
-- through the import graph.

module Crisp.Module.Import
  ( -- * Parsing
    parseModulePath
  , parseImport
  , parseQualifiedName
  , parseReExport
    -- * Resolution
  , resolveName
  , resolveQualifiedName
  , checkAmbiguity
  , resolveReExports
    -- * Construction
  , makeQualifiedName
    -- * Cycle Detection
  , checkCycles
  ) where

import Crisp.Module.Types

import Control.Monad (guard)
import Data.Char (isAsciiUpper, isAsciiLower, isDigit)
import Data.List (find, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- =============================================================================
-- Parsing
-- =============================================================================

-- | Parse a module path from text
parseModulePath :: Text -> Either Text ModPath
parseModulePath txt
  | T.null txt = Left "Empty module path"
  | T.head txt == '.' = Left "Module path cannot start with dot"
  | T.last txt == '.' = Left "Module path cannot end with dot"
  | otherwise =
      let segments = T.splitOn "." txt
      in if all isValidSegment segments
         then Right (ModPath segments)
         else Left "Invalid module path: segments must start with uppercase"
  where
    isValidSegment seg
      | T.null seg = False
      | otherwise =
          let c = T.head seg
          in isAsciiUpper c && T.all isValidChar seg

    isValidChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c == '_' || c == '\''

-- | Parse an import declaration from text
parseImport :: Text -> Either Text ImportDecl
parseImport txt = do
  let stripped = T.strip txt
  -- Check for "import" keyword
  rest <- stripPrefix "import " stripped
  let rest' = T.strip rest

  -- Check for "qualified" keyword
  if "qualified " `T.isPrefixOf` rest'
    then parseQualifiedImport (T.strip $ T.drop 10 rest')
    else parseUnqualifiedImport rest'

parseQualifiedImport :: Text -> Either Text ImportDecl
parseQualifiedImport txt = do
  -- Format: ModPath as Alias
  case T.breakOn " as " txt of
    (modPathTxt, asClause)
      | T.null asClause -> Left "Qualified import requires 'as' clause"
      | otherwise -> do
          modPath <- parseModulePath (T.strip modPathTxt)
          let alias = T.strip $ T.drop 4 asClause  -- drop " as "
          if T.null alias || not (isAsciiUpper (T.head alias))
            then Left "Alias must be uppercase identifier"
            else Right $ ImportDecl modPath (Qualified alias)

parseUnqualifiedImport :: Text -> Either Text ImportDecl
parseUnqualifiedImport txt
  | T.null txt = Left "Import requires module name"
  | otherwise = do
      -- Check for hiding or selective
      case T.breakOn " hiding " txt of
        (modPathTxt, hidingClause)
          | not (T.null hidingClause) -> do
              modPath <- parseModulePath (T.strip modPathTxt)
              names <- parseNameList (T.strip $ T.drop 8 hidingClause)
              Right $ ImportDecl modPath (Hiding names)
          | otherwise ->
              case T.breakOn " (" txt of
                (modPathTxt', selectiveClause)
                  | not (T.null selectiveClause) -> do
                      modPath <- parseModulePath (T.strip modPathTxt')
                      names <- parseNameList (T.strip selectiveClause)
                      Right $ ImportDecl modPath (Selective names)
                  | otherwise -> do
                      modPath <- parseModulePath (T.strip txt)
                      Right $ ImportDecl modPath Unqualified

-- | Parse a parenthesized name list
parseNameList :: Text -> Either Text (Set Text)
parseNameList txt
  | T.null txt = Left "Empty name list"
  | T.head txt /= '(' = Left "Name list must start with '('"
  | T.last txt /= ')' = Left "Name list must end with ')'"
  | otherwise =
      let inner = T.strip $ T.dropEnd 1 $ T.drop 1 txt
          names = map T.strip $ T.splitOn "," inner
      in if all isValidName names
         then Right (Set.fromList names)
         else Left "Invalid name in import list"
  where
    isValidName n
      | T.null n = False
      | otherwise =
          let c = T.head n
          in (isAsciiLower c || isAsciiUpper c || c == '_') && T.all isNameChar n

    isNameChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c == '_' || c == '\''

-- | Parse a qualified name (e.g., "M.foo" or "Core.Prelude.map")
parseQualifiedName :: Text -> Either Text (Text, Text)
parseQualifiedName txt = do
  let parts = T.splitOn "." txt
  case reverse parts of
    [] -> Left "Empty qualified name"
    [_] -> Left "Qualified name requires at least one dot"
    (name:qualParts)
      | T.null name -> Left "Qualified name cannot end with dot"
      | any T.null qualParts -> Left "Empty qualifier segment"
      | otherwise ->
          Right (T.intercalate "." (reverse qualParts), name)

-- | Parse a re-export declaration
parseReExport :: Text -> Either Text ReExport
parseReExport txt = do
  let stripped = T.strip txt
  rest <- stripPrefix "export " stripped
  let rest' = T.strip rest

  if "module " `T.isPrefixOf` rest'
    then do
      modPath <- parseModulePath (T.strip $ T.drop 7 rest')
      Right $ ReExport modPath ReExportAll
    else do
      -- Format: ModPath (names)
      case T.breakOn " (" rest' of
        (modPathTxt, selectiveClause)
          | not (T.null selectiveClause) -> do
              modPath <- parseModulePath (T.strip modPathTxt)
              names <- parseNameList (T.strip selectiveClause)
              Right $ ReExport modPath (ReExportSome names)
          | otherwise -> Left "Re-export requires 'module' keyword or name list"

-- | Helper to strip a prefix from text
stripPrefix :: Text -> Text -> Either Text Text
stripPrefix prefix txt
  | prefix `T.isPrefixOf` txt = Right (T.drop (T.length prefix) txt)
  | otherwise = Left $ "Expected '" <> prefix <> "'"

-- =============================================================================
-- Resolution
-- =============================================================================

-- | Resolve an unqualified name through imports
resolveName :: [ImportDecl] -> Map ModPath (Set Text) -> Text -> Maybe (ModPath, Text)
resolveName imports available name =
  listToMaybe $ mapMaybe resolveInImport imports
  where
    resolveInImport (ImportDecl modPath form) = do
      -- Check if this import makes the name available
      guard $ case form of
        Qualified _ -> False  -- Qualified imports don't provide unqualified access
        Selective names -> Set.member name names
        Hiding hidden -> not (Set.member name hidden) && nameAvailable modPath
        Unqualified -> nameAvailable modPath

      pure (modPath, name)

    nameAvailable modPath =
      case Map.lookup modPath available of
        Just names -> Set.member name names
        Nothing -> False

-- | Resolve a qualified name through imports
resolveQualifiedName :: [ImportDecl] -> Map ModPath (Set Text) -> Text -> Text -> Maybe (ModPath, Text)
resolveQualifiedName imports available qualifier name = do
  -- Find the import with matching qualifier
  ImportDecl modPath _ <- find matchesQualifier imports
  -- Check name is available in that module
  moduleNames <- Map.lookup modPath available
  guard $ Set.member name moduleNames
  pure (modPath, name)
  where
    matchesQualifier (ImportDecl _ (Qualified alias)) = alias == qualifier
    matchesQualifier _ = False

-- | Check for ambiguous imports
checkAmbiguity :: [ImportDecl] -> Map ModPath (Set Text) -> Text -> Maybe [ModPath]
checkAmbiguity imports available name =
  let sources = mapMaybe (providesName name) imports
  in if length sources > 1
     then Just (sortBy (comparing (length . modPathSegments)) sources)
     else Nothing
  where
    providesName n (ImportDecl modPath form) = do
      -- Check if this import makes the name available unqualified
      guard $ case form of
        Qualified _ -> False
        Selective names -> Set.member n names
        Hiding hidden -> not (Set.member n hidden) && nameAvailable modPath
        Unqualified -> nameAvailable modPath
      pure modPath

    nameAvailable modPath =
      case Map.lookup modPath available of
        Just names -> Set.member name names
        Nothing -> False

-- | Resolve re-exports into a set of exported names
resolveReExports :: [ReExport] -> Map ModPath (Set Text) -> Set Text
resolveReExports reexports available =
  Set.unions $ map resolveReExport' reexports
  where
    resolveReExport' (ReExport modPath ReExportAll) =
      Map.findWithDefault Set.empty modPath available
    resolveReExport' (ReExport modPath (ReExportSome names)) =
      let moduleNames = Map.findWithDefault Set.empty modPath available
      in Set.intersection names moduleNames

-- =============================================================================
-- Construction
-- =============================================================================

-- | Construct a fully qualified name
makeQualifiedName :: ModPath -> Text -> Text
makeQualifiedName (ModPath segs) name =
  T.intercalate "." segs <> "." <> name

-- =============================================================================
-- Cycle Detection
-- =============================================================================

-- | Check for circular imports in a dependency graph
-- Returns Just the cycle path if found, Nothing otherwise
checkCycles :: Map ModPath [ModPath] -> Maybe [ModPath]
checkCycles deps
  | Map.null deps = Nothing
  | otherwise =
      let nodes = Map.keys deps
      in listToMaybe $ mapMaybe (findCycleFrom deps Set.empty []) nodes

-- | Find a cycle starting from a node
findCycleFrom :: Map ModPath [ModPath] -> Set ModPath -> [ModPath] -> ModPath -> Maybe [ModPath]
findCycleFrom deps visited path node
  | node `Set.member` visited =
      -- Found a cycle - return the path from the repeated node
      Just (dropWhile (/= node) path ++ [node])
  | otherwise =
      let newVisited = Set.insert node visited
          newPath = path ++ [node]
          children = Map.findWithDefault [] node deps
      in listToMaybe $ mapMaybe (findCycleFrom deps newVisited newPath) children
