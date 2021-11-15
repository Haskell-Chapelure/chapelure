{-# LANGUAGE LambdaCase #-}
module Chapelure.Handler.PlainText where

import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector.NonEmpty (NonEmptyVector)
import Optics.Core
import Prettyprinter (Doc, pretty, indent, space, emptyDoc, concatWith, surround, hardline, layoutPretty, defaultLayoutOptions, hcat)
import Prettyprinter.Render.Text (renderStrict)
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Data.Vector.NonEmpty as NEVec

import Chapelure.Types
import Chapelure.Errors

render :: Diagnostic -> Text
render diag@Diagnostic{..} = do
  let header = renderHeader severity
  let footer = renderStrict $ layoutPretty defaultLayoutOptions $ renderFooter diag
  case snippets of
    Nothing -> T.stripEnd . T.unlines . filterEmpty $ [header, " ", footer]
    Just sn ->
      let snipText =  (T.stripEnd . T.unlines . filterEmpty $ vectorFoldMap' (\b -> [b]) $ renderSnippets sn) <> "\n"
     in T.stripEnd . T.unlines . filterEmpty $ [header, " ", snipText, footer]

renderHeader :: Severity -> Text
renderHeader severity = "Diagnostic severity: " <> display severity

renderSnippets :: NonEmptyVector Snippet -> Vector Text
renderSnippets snippets = NEVec.toVector $ fmap renderOne snippets

renderOne :: Snippet -> Text
renderOne snippet = renderStrict $ layoutPretty defaultLayoutOptions $
  renderSnippet (groupSnippetHighlights snippet) snippet

renderFooter :: Diagnostic -> Doc ()
renderFooter Diagnostic{..} =
  concatWith (surround hardline) [ helpMessage, codeMessage, linkMessage ]
  where
    helpMessage = maybe "" (\m -> "Help: " <> pretty m) help
    codeMessage = maybe "" (\c -> "Code: " <> pretty c) code
    linkMessage = maybe "" (\u -> "Link: " <> pretty u) link

-- | 1. Take a snippet
-- 2. Display relevant location information in its header
-- 3. Iterate over its content's lines (with their index)
-- 4. If there is an associated line in the map, render the pointer of this line alongside with the line
-- 5. If there is none, just render the line
-- 6. profit I guess…
--
-- -> Code.hs:3:10
-- 3 │ add :: Int
--            ^^^ Return type is an Int
-- 4 │ add = 1 + True
--               ^^^^ Second argument is a Bool
--             ^ This takes an Int
--

renderSnippet :: Map Line (NonEmptyVector Source) -- ^
               -> Snippet -- ^
               -> Doc ()
renderSnippet highlightsMap snippet =
  concatWith (surround hardline) [ snippetHeader (snippet ^. #location), snippetBody ]
  where
    snippetHeader :: (Text, Line, Column) -> Doc ()
    snippetHeader (location, line, column) = "-> " <> pretty location <> ":" <> pretty line <> ":" <> pretty column

    snippetBody :: Doc ()
    snippetBody = concatWith (surround hardline) $ vectorFoldMap' (\b -> [b]) results

    results :: Vector (Doc ())
    results = Vec.imap processHighlight (snippet ^. #content)

    -- This function must increment the initial index to 1 in order not to have snippets
    -- starting at line 0!
    processHighlight :: Int -- ^ Index of the source line
                     -> Text -- ^ Content of the source line
                     -> Doc ()
    processHighlight index s =
      let source = pretty s
          (_, Line startLine, _) = snippet ^. #location
          lineNumber = Line (intToWord index + startLine)
       in case Map.lookup lineNumber highlightsMap of
            Nothing -> enumerate lineNumber [source]
            Just highlights ->
              let
                  ps = concatWith (surround hardline) $ NEVec.toList $ fmap mkPointer highlights
                  finalLine = enumerate lineNumber [source]
               in finalLine <> "\n" <> ps

checkHighlight :: Source -> Maybe ChapelureError
checkHighlight Source{label, startColumn=(Column startColLine), endColumn=(Column endColLine)} =
  case compare (endColLine - startColLine) endColLine of
    GT -> Just $ SpanOutOfBounds (fromMaybe "<source>" label) (Column startColLine, Column endColLine)
    _  -> Nothing

-- | >>> mkPointer Source{ label = Just "Foobar", line = Line 3, startColumn = Column 7, endColumn = Column 10 }
--            ^ Foobar^ Foobar^ Foobar
mkPointer :: Source -> Doc ()
mkPointer s =
  let Source{line, label, startColumn=(Column startColLine), endColumn=(Column endColLine)} = s
      marginPad = marginSize line + startColLine
      carets = indent (wordToInt marginPad) $
        hcat $ replicate (wordToInt . noZero $ endColLine - startColLine) (pretty @Text "^")
   in carets <> space <> maybe emptyDoc pretty label

-- helpers

noZero :: Word -> Word
noZero x | x == 0 = 1
         | otherwise = x

marginSize :: Line -> Word
marginSize line = intToWord $ T.length $ display line <> " │ "

enumerate :: Line -> [Doc ()] -> Doc ()
enumerate startLine docs = go startLine docs ""
  where
    go :: Line -> [Doc ()] -> Doc () -> Doc ()
    go _startLine' [] acc = acc
    go startLine' (x:xs) acc = go (incrementLine startLine) xs (acc <> pretty startLine' <> " │ " <> x)

-- | Group a snippet's highlights by their line
groupSnippetHighlights :: Snippet -> Map Line (NonEmptyVector Source)
groupSnippetHighlights snippet =
  case snippet ^. #highlights of
    Just hs ->
      Map.fromListWith (<>) $ map (\source -> (line source, NEVec.singleton source) )
        $ NEVec.toList hs
    Nothing -> Map.empty

---- Helpers

intToWord :: Int -> Word
intToWord = fromIntegral

wordToInt :: Word -> Int
wordToInt = fromIntegral

filterEmpty :: [Text] -> [Text]
filterEmpty list = filter (not . T.null) list

nonEmptyVectorFoldMap' :: (Monoid m) => (a -> m) -> NonEmptyVector a -> m
nonEmptyVectorFoldMap' f = NEVec.foldl' (\acc a -> acc `mappend` f a) mempty

vectorFoldMap' :: (Monoid m) => (a -> m) -> Vector a -> m
vectorFoldMap' f = Vec.foldl' (\acc a -> acc `mappend` f a) mempty

lefts :: Vector (Either a b) -> Vector a
lefts = Vec.fromList . Either.lefts . Vec.toList

rights :: Vector (Either a b) -> Vector b
rights = Vec.fromList . Either.rights . Vec.toList
