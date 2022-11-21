{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | A pretty renderer for 'Diagnostic's. Can optionally render in color
module Chapelure.Handler.Colourful
  ( render
  , Config (..)
  , prettyConfig
  , asciiColorConfig
  , asciiPlainConfig
  ) where

import Chapelure.Style
  ( DocText
  , Style
  , StyleColor (Color16, Color256, ColorRGB)
  , styleFG
  , styleUnderline
  )
import Chapelure.Types
  ( Column (..)
  , Diagnostic (Diagnostic)
  , Highlight (Highlight, spans)
  , Line (..)
  , Severity (Error, Info, Warning)
  , Snippet (Snippet)
  )
import Data.Bifunctor (Bifunctor (first))
import Data.Colour (Colour, colourConvert)
import Data.Fixed (mod')
import Data.Foldable (Foldable (fold), toList)
import Data.List (mapAccumL)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Semigroup (Semigroup (stimes))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as DVNE
import HSLuv
  ( HSLuv (HSLuv)
  , HSLuvHue (HSLuvHue)
  , HSLuvLightness (HSLuvLightness)
  , HSLuvSaturation (HSLuvSaturation)
  , hsluvToColour
  )
import Optics.Core ((<&>))
import Prettyprinter (LayoutOptions (LayoutOptions, layoutPageWidth), PageWidth (AvailablePerLine, Unbounded), Pretty (pretty), SimpleDocStream (..), annotate, brackets, hardline, indent, layoutPretty, space)
import System.Console.ANSI (Color (Blue, Red, Yellow), ColorIntensity (Vivid), Underlining (SingleUnderline), xterm24LevelGray)

-- Color generation
-- https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
-- lightness between 0 and 100
spacedColors :: Double -> [Colour Double]
spacedColors lightness = map (\hue -> hsluvToColour $ HSLuv (HSLuvHue $ hue * 360.0) (HSLuvSaturation 100.0) (HSLuvLightness lightness)) hues
  where
    goldenRatioConjugate :: Double
    goldenRatioConjugate = 0.618033988749895

    hues = map (\x -> (x * goldenRatioConjugate) `mod'` 1.0) [1 ..]

headMempty :: Monoid a => [a] -> a
headMempty [] = mempty
headMempty (a : _as) = a

-- | Configuration for rendering
data Config = Config
  { tabWidth :: !Word
  , layoutOptions :: !LayoutOptions
  , infoStyle :: !Style
  , warningStyle :: !Style
  , errorStyle :: !Style
  , gutterStyle :: !Style
  , linkStyle :: !Style
  , colourizeHighlights :: !Bool
  , gutterVLine :: !Char
  , gutterVBreak :: !Char
  , gutterHLineHead :: !Char
  , gutterHLineFoot :: !Char
  , gutterCornerHead :: !Char
  , gutterCornerFoot :: !Char
  , locationLeftBracket :: !Text
  , locationRightBracket :: !Text
  , locationSeparator :: !Text
  , highlightUnderLeft :: !Char
  , highlightUnderRight :: !Char
  , highlightUnder :: !Char
  , highlightUnderDown :: !Char
  , highlightTwospan :: !Char
  , highlightConnector :: !Char
  , highlightConnectionLeft :: !Char
  , highlightConnectionRight :: !Char
  , highlightHConnector :: !Char
  , highlightHTee :: !Char
  , highlightLabelTee :: !Char
  , highlightCornerTop :: !Char
  , highlightCornerBottom :: !Char
  }

prettyConfig, asciiColorConfig, asciiPlainConfig :: Config

-- | A pretty rendering configuration, making use of color and Unicode box-drawing characters
prettyConfig =
  Config
    { tabWidth = 4
    , layoutOptions = LayoutOptions (AvailablePerLine 80 1.0)
    , infoStyle = styleFG $ Color16 Vivid Blue
    , warningStyle = styleFG $ Color16 Vivid Yellow
    , errorStyle = styleFG $ Color16 Vivid Red
    , gutterStyle = styleFG (Color256 $ xterm24LevelGray 12)
    , linkStyle = styleUnderline SingleUnderline
    , colourizeHighlights = True
    , gutterVLine = '│'
    , gutterVBreak = '┆'
    , gutterHLineHead = ' '
    , gutterHLineFoot = ' '
    , gutterCornerHead = '╭'
    , gutterCornerFoot = '╯'
    , locationLeftBracket = "["
    , locationRightBracket = "]"
    , locationSeparator = ":"
    , highlightUnderLeft = '╰'
    , highlightUnderRight = '╯'
    , highlightUnder = '─'
    , highlightUnderDown = '┬'
    , highlightTwospan = '├'
    , highlightConnector = '│'
    , highlightConnectionLeft = '╯'
    , highlightConnectionRight = '╰'
    , highlightHConnector = '─'
    , highlightHTee = '├'
    , highlightLabelTee = '┴'
    , highlightCornerTop = '╭'
    , highlightCornerBottom = '╰'
    }

-- | A colorful ASCII rendering configuration
asciiColorConfig =
  prettyConfig
    { gutterVLine = '|'
    , gutterVBreak = ':'
    , gutterCornerHead = '/'
    , gutterCornerFoot = '/'
    , highlightUnderLeft = '^'
    , highlightUnderRight = '^'
    , highlightUnder = '^'
    , highlightUnderDown = '^'
    , highlightTwospan = '^'
    , highlightConnector = '|'
    , highlightConnectionLeft = '/'
    , highlightConnectionRight = '\\'
    , highlightHConnector = '-'
    , highlightHTee = '|'
    , highlightLabelTee = '*'
    , highlightCornerTop = '/'
    , highlightCornerBottom = '\\'
    }

-- | A plain ASCII rendering configuration. Useful if the terminal supports neither colour nor box-drawing characters
asciiPlainConfig =
  asciiColorConfig
    { infoStyle = mempty
    , warningStyle = mempty
    , errorStyle = mempty
    , gutterStyle = mempty
    , linkStyle = mempty
    , colourizeHighlights = False
    }

-- | What to render at the left of the line
data Gutter
  = --      ╭[source:line:column]
    Header (Maybe Text) Line Column
  | --      |
    HeaderSpace
  | -- line |
    Numbered Line
  | --      |
    Unnumbered (Maybe Semantic)
  | --      ┆
    Break
  | --      ╯
    Footer

-- | The meaning of an unnumbered line: used to connect up multi-line highlights
data Semantic
  = -- | Extra information in the footer
    FooterInfo
  | -- | Line contains the first connnector for highlight #[Int]
    FirstConnectorFor Int
  | -- | Line contains a connnector for highlight #[Int]
    MidConnectorFor Int
  | -- | Line contains the last connnector for highlight #[Int]
    LastConnectorFor Int

-- | What kind of multi-span label to render in a column
data MultiSpan
  = -- | No multi-span label here
    NoMulti
  | -- | A vertical connection
    Vee Style
  | -- | A vertical-right connection
    Tee Style TeeLocation

-- | Where a tee-junction of a multi-span is within the multi-span
data TeeLocation
  = TeeTop
  | TeeMid
  | TeeBottom

-- | A connector to labels and/or multi-spans
data Connector
  = -- | ╯ Connect to a label on the left
    LeftConnector
  | -- | ╰ Connect to a label on the right
    RightConnector
  | -- | ╯ Connect to a multi-span
    MultiConnector
  | -- | ┴ Connect to a multi-span and a label on the right
    MultiEndConnector

-- | How to render a particular line, and the line's semantics
data RenderLine = RenderLine
  { gutter :: Gutter
  , multispans :: [MultiSpan]
  , contentOffset :: Word
  , renderedContent :: [(Text, Style)]
  , connector :: Maybe (Style, Connector)
  }

renderLine :: Config -> Word -> RenderLine -> DocText
renderLine config pad (RenderLine g ms co rc conn) =
  goGutter g
    <> goMulti ms co
    <> case conn of
      Just (connSt, RightConnector) -> annotate connSt $ pretty (highlightConnectionRight config)
      Just (connSt, MultiConnector) -> annotate connSt $ pretty (highlightConnectionLeft config)
      Just (connSt, MultiEndConnector) -> annotate connSt $ pretty (highlightLabelTee config)
      _ -> mempty
    <> space
    <> goContent rc
    <> case conn of
      Just (connSt, LeftConnector) -> space <> annotate connSt (pretty (highlightConnectionLeft config))
      _ -> mempty
    <> hardline
  where
    goGutter :: Gutter -> DocText
    goGutter =
      annotate (gutterStyle config)
        . \case
          Header source l c' ->
            stimes pad (pretty $ gutterHLineHead config)
              <> pretty (gutterCornerHead config)
              <> pretty (locationLeftBracket config)
              <> maybe mempty (\s -> pretty s <> pretty (locationSeparator config)) source
              <> pretty l
              <> pretty (locationSeparator config)
              <> pretty c'
              <> pretty (locationRightBracket config)
          HeaderSpace ->
            indent (fromIntegral pad) (pretty $ gutterVLine config)
          Numbered li ->
            let s = display li
             in indent (fromIntegral pad - T.length s) $ pretty s <> pretty (gutterVLine config)
          Unnumbered _ -> indent (fromIntegral pad) $ pretty (gutterVLine config)
          Break -> indent (fromIntegral pad) $ pretty (gutterVBreak config)
          Footer ->
            stimes pad (pretty $ gutterHLineFoot config)
              <> pretty (gutterCornerFoot config)

    goMulti :: [MultiSpan] -> Word -> DocText
    goMulti ms' co' =
      let (hl, docs) =
            mapAccumL
              ( \hl' -> \case
                  NoMulti -> (hl', maybe space (\st -> annotate st $ pretty (highlightHConnector config)) hl')
                  Vee st -> (hl', annotate st $ pretty (highlightConnector config))
                  Tee st TeeTop -> (Just st, annotate st $ pretty (highlightCornerTop config))
                  Tee st TeeMid -> (Just st, annotate st $ pretty (highlightHTee config))
                  Tee st TeeBottom -> (Just st, annotate st $ pretty (highlightCornerBottom config))
              )
              Nothing
              ms'
       in fold docs <> stimes co' (maybe space (\st -> annotate st $ pretty (highlightHConnector config)) hl)

    goContent :: [(Text, Style)] -> DocText
    goContent = foldMap (\(t, s) -> annotate s (pretty t))

buildDoc :: LayoutOptions -> DocText -> Seq [(Text, Style)]
buildDoc lo = go [] [] . layoutPretty lo
  where
    go :: [Style] -> [(Text, Style)] -> SimpleDocStream Style -> Seq [(Text, Style)]
    go st line = \case
      SFail -> [reverse line]
      SEmpty -> [reverse line]
      SChar c sds -> go st ((T.singleton c, headMempty st) : line) sds
      SText _n txt sds -> go st ((txt, headMempty st) : line) sds
      SLine n sds -> reverse line :<| go st [(T.replicate n " ", headMempty st)] sds
      SAnnPush st' sds -> case st of
        [] -> go [st'] line sds
        (s : ss) -> go (s <> st' : s : ss) line sds
      SAnnPop sds -> go (drop 1 st) line sds

buildGutter :: LayoutOptions -> Line -> Line -> Vector DocText -> (Word, Seq RenderLine)
buildGutter lo s e lines' = (fromIntegral pad, body)
  where
    pad = T.length (display e)
    padded = LayoutOptions $ case layoutPageWidth lo of
      AvailablePerLine n x -> AvailablePerLine (n - pad) x
      Unbounded -> Unbounded

    body =
      Seq.zip [s .. e] (Seq.fromList $ toList lines') >>= \(i, line) ->
        fmap (\r -> RenderLine (Numbered i) [] 0 r Nothing) (buildDoc padded line)

buildGutter' :: Bool -> Style -> LayoutOptions -> Maybe Text -> Line -> Column -> Line -> Vector DocText -> Maybe DocText -> Maybe Text -> (Word, Seq RenderLine)
buildGutter' isFinal linkStyle lo source s sc e lines' me li =
  ( pad
  , go (Header source s sc)
      :<| go HeaderSpace
      :<| ( bg
              <> if not isFinal
                then mempty
                else
                  ((\l -> RenderLine (Unnumbered (Just FooterInfo)) [] 0 l Nothing) <$> buildDoc lo (fromMaybe mempty me))
                    <> if not isFinal then mempty else maybe mempty (\link -> [RenderLine (Unnumbered (Just FooterInfo)) [] 0 [(link, linkStyle)] Nothing]) li :|> go Footer
          )
  )
  where
    go g = RenderLine g [] 0 [] Nothing
    (pad, bg) = buildGutter lo s e lines'

highlightSpans :: [(Column, Column, Style)] -> [(Text, Style)] -> [(Text, Style)]
highlightSpans hls = go 1
  where
    go :: Word -> [(Text, Style)] -> [(Text, Style)]
    go _col [] = []
    go col ((t, st) : rest) =
      let cs = zipWith go' [col ..] (map (,st) (T.unpack t))
       in coalesce (map (first T.singleton) cs) ++ go (col + fromIntegral (T.length t)) rest

    coalesce :: [(Text, Style)] -> [(Text, Style)]
    coalesce ((t, st) : (t', st') : rest) | st == st' = coalesce ((t <> t', st') : rest)
    coalesce (a : as) = a : coalesce as
    coalesce [] = []

    go' :: Word -> (Char, Style) -> (Char, Style)
    go' col (c, s) =
      ( c
      , s
          <> fold
            ( mapMaybe
                (\(s', e, st) -> if s' <= Column col && Column col <= e then Just st else Nothing)
                hls
            )
      )

data MultiKind
  = Single (Maybe DocText)
  | Multi
  | MultiEnd (Maybe DocText)

data RenderHighlight = RenderHighlight
  { hiStart :: Column
  , hiEnd :: Column
  , hiStyle :: Style
  , multiKind :: MultiKind
  , hiSem :: Maybe Semantic
  }

underlineHighlight :: Config -> [RenderHighlight] -> Seq RenderLine
underlineHighlight config highlights = Seq.fromList $ highlights >>= go
  where
    go (RenderHighlight (Column cs') (Column ce') st mk sem) =
      let mid = (cs' + ce') `div` 2
          underline =
            if
                | ce' == cs' -> T.singleton $ highlightUnderDown config
                | ce' == cs' + 1 -> T.singleton (highlightTwospan config) <> T.singleton (highlightUnderRight config)
                | otherwise ->
                    T.singleton (highlightUnderLeft config)
                      <> T.replicate (fromIntegral $ mid - cs' - 1) (T.singleton $ highlightUnder config)
                      <> T.singleton (highlightUnderDown config)
                      <> T.replicate (fromIntegral $ ce' - mid - 1) (T.singleton $ highlightUnder config)
                      <> T.singleton (highlightUnderRight config)
          connector
            | Multi <- mk = MultiConnector
            | MultiEnd _ <- mk = MultiEndConnector
            | Unbounded <- layoutPageWidth (layoutOptions config) = RightConnector
            | AvailablePerLine n _x <- layoutPageWidth $ layoutOptions config
            , n `div` 2 > fromIntegral mid =
                RightConnector
            | otherwise = LeftConnector
          t = case mk of
            Single d -> d
            Multi -> Nothing
            MultiEnd d -> d
          z = zip (Just connector : repeat Nothing) (toList (buildDoc (layoutOptions config) (annotate st $ fromMaybe mempty t)))
       in RenderLine (Unnumbered Nothing) [] (cs' - 1) [(underline, st)] Nothing
            : (z <&> \(conn, l) -> RenderLine (if isJust conn then Unnumbered sem else Unnumbered Nothing) [] mid l ((st,) <$> conn))

-- | Renders a 'Diagnostic' using the provided 'Config' into a 'DocText'.
render :: Config -> Diagnostic -> DocText
render config (Diagnostic co se me he li snip) =
  let sgr = case se of
        Info -> infoStyle config
        Warning -> warningStyle config
        Error -> errorStyle config

      code =
        co <&> \co' ->
          annotate sgr $
            brackets (pretty co') <> ": "
      label' = annotate sgr $ pretty (show se) <> ": "
      mess = fromMaybe mempty me <> hardline
      snips :: Vector Snippet
      snips = maybe mempty DVNE.toVector snip
      body = foldMap go (zip [0 ..] (toList snips))
      inCaseNoSnippets = maybe showFooter (const mempty) snip
   in fromMaybe mempty code <> label' <> mess <> body <> inCaseNoSnippets
  where
    showFooter =
      foldMap (foldMap (\(t, s) -> annotate s (pretty t) <> hardline)) $
        buildDoc (layoutOptions config) (fromMaybe mempty he)
          <> maybe mempty (\link -> [[(link, linkStyle config)]]) li

    go (i, Snippet (source, ln@(Line ln'), col) highlights' lines') =
      foldMap
        (renderLine config pad)
        ( renderMulti $
            fullGutter >>= \rl ->
              case gutter rl of
                Numbered li' -> rl :<| maybe mempty (underlineHighlight config) (Map.lookup li' highlights)
                _ -> [rl]
        )
      where
        hls =
          zip
            (if colourizeHighlights config then map (styleFG . ColorRGB . colourConvert) (spacedColors 75.0) else repeat mempty)
            (maybe mempty toList highlights')

        fromList' :: Ord k => [(k, v)] -> Map k [v]
        fromList' = foldr (\(k, v) m -> Map.insertWith (<>) k [v] m) mempty

        highlights =
          fromList' $
            concatMap
              ( \(name, (style, Highlight label' spans)) ->
                  ( \(j :: Integer, (l, cs, ce)) ->
                      ( l
                      , RenderHighlight
                          cs
                          ce
                          style
                          if
                              | DVNE.length spans >= 2, j == fromIntegral (DVNE.length spans - 1) -> MultiEnd label'
                              | DVNE.length spans >= 2 -> Multi
                              | otherwise -> Single label'
                          if
                              | j == 0 -> Just $ FirstConnectorFor name
                              | j == fromIntegral (DVNE.length spans - 1) -> Just $ LastConnectorFor name
                              | otherwise -> Just $ MidConnectorFor name
                      )
                  )
                    <$> zip [0 ..] (toList spans)
              )
              (zip [0 ..] hls)

        multilineHighlights :: [((Int, Style), [(Line, Column, Column)])]
        multilineHighlights = mapMaybe (\(k, (st, hi)) -> if DVNE.length (spans hi) >= 2 then Just ((k, st), toList (spans hi)) else Nothing) (zip [0 ..] hls)
        multilineHighlights' :: [Map Line [(Column, Column)]]
        multilineHighlights' = fmap (fromList' . fmap (\(l, cs, ce) -> (l, (cs, ce))) . snd) multilineHighlights
        multilineHighlights'' :: [Set Line]
        multilineHighlights'' =
          fmap
            ( \m -> case (Map.lookupMin m, Map.lookupMax m) of
                (Just (mini, _), Just (maxi, _)) -> Set.fromList [mini .. maxi]
                _ -> mempty
            )
            multilineHighlights'
        multilineHighlights''' :: [((Int, Style), Set Line)]
        multilineHighlights''' = zip (fmap fst multilineHighlights) multilineHighlights''

        (pad, gutter') = buildGutter' (i == maybe 0 DVNE.length snip - 1) (linkStyle config) (layoutOptions config) source ln col (Line $ ln' + fromIntegral (V.length lines')) lines' he li

        forget = fmap (\(RenderHighlight cs ce st _ _) -> (cs, ce, st))
        hlGutter =
          gutter' <&> \rl -> case gutter rl of
            Numbered li' -> rl{renderedContent = maybe id (highlightSpans . forget) (Map.lookup li' highlights) $ renderedContent rl}
            _ -> rl
        fullGutter = if colourizeHighlights config then hlGutter else gutter'

        renderMulti :: Seq RenderLine -> Seq RenderLine
        renderMulti s =
          foldr
            ( \((i', st), _ls) ls' ->
                snd $
                  mapAccumL
                    ( \isStyled rl ->
                        let (isStyled', c) = case gutter rl of
                              Unnumbered (Just (FirstConnectorFor j)) | i' == j -> (True, Tee st TeeTop)
                              Unnumbered (Just (MidConnectorFor j)) | i' == j -> (True, Tee st TeeMid)
                              Unnumbered (Just (LastConnectorFor j)) | i' == j -> (False, Tee st TeeBottom)
                              Unnumbered (Just FooterInfo) -> (False, NoMulti)
                              Footer -> (False, NoMulti)
                              _ -> (isStyled, if isStyled then Vee st else NoMulti)
                         in (isStyled', rl{multispans = c : multispans rl})
                    )
                    False
                    ls'
            )
            s
            multilineHighlights'''

-- ──────────────────────────────── Testing ──────────────────────────────── --
-- colourTest :: IO ()
-- colourTest = go (zip (spacedColors 75.0) (words "This is a colour test! Fugiat excepturi illo nihil porro voluptatem. Repellendus velit quibusdam aut dolorem. Quis non ipsa qui molestiae explicabo quos. Sed dolorum laborum expedita exercitationem sit quaerat veniam culpa. Voluptas quas molestiae earum delectus veniam officia ut ut. Cupiditate tenetur libero quibusdam maiores ea. Est consequuntur perferendis est optio officia aliquid ratione. Velit hic eaque qui voluptatibus ipsam. Rem facilis temporibus corporis quia perferendis. Commodi vero laborum esse voluptas est numquam. Laudantium fuga aliquam repudiandae explicabo ut dolores beatae. Pariatur et provident consequatur optio neque asperiores voluptatem necessitatibus. Iusto nam sint et. Non qui cupiditate porro corporis qui. Dignissimos voluptates iusto dolor. Itaque minus et sed qui non quidem. Perspiciatis omnis id eaque. Quis rerum architecto magni qui iure nisi voluptatum. Omnis esse pariatur pariatur non. Fugiat provident voluptate maxime cupiditate unde consequatur at id. Praesentium molestiae consectetur sequi dolor qui nulla vel fuga. Enim aut assumenda recusandae. Et quisquam architecto quasi aut nihil unde et dolores. Delectus qui esse sapiente ut. Eum ut quis expedita reprehenderit et nihil odio sint. Molestias quidem iusto delectus est consequatur voluptas possimus. Neque reiciendis maiores cumque a non nihil."))
--   where
--     go :: [(Colour Double, String)] -> IO ()
--     go [] = putStrLn "" >> setSGR []
--     go ((c, s) : rest) = do
--       setSGR [SetRGBColor Foreground (colourConvert c)]
--       putStr s
--       putStr " "
--       go rest

-- displayDiag :: Diagnostic -> IO ()
-- displayDiag = render asciiColorConfig >>> layoutPretty defaultLayoutOptions >>> putDocText

-- testDiag :: Diagnostic
-- testDiag =
--   Diagnostic
--     (Just "E0001")
--     Error
--     (Just "Found a bug!")
--     (Just "Note: your code is broken!")
--     (Just "https://sofia.sofia")
--     ( DVNE.fromVector
--         [ Snippet
--             (Just "sofia.ð", Line 99, Column 1)
--             ( DVNE.fromVector
--                 [ Highlight (Just "unrecognized type 'imt'") (DVNE.singleton (Line 99, Column 13, Column 15)),
--                   Highlight
--                     (Just "note: maybe you mean 'int' to match with y?")
--                     ( DVNE.singleton (Line 100, Column 9, Column 13) <> DVNE.singleton (Line 101, Column 9, Column 9)
--                     )
--                 ]
--             )
--             [ "def blah(x: imt): int",
--               "        y = x",
--               "        y"
--             ]
--         ]
--     )

-- testDiag2 :: Diagnostic
-- testDiag2 =
--   Diagnostic
--     (Just "E0002")
--     Info
--     (Just $ "Edge case " <> annotate (styleItalicized True) "testing!")
--     (Just ("Note: we can have " <> annotate (styleFG (Color16 Vivid Cyan)) "colors!"))
--     (Just "https://sofia.sofia")
--     ( DVNE.fromVector
--         [ Snippet
--             (Just "sofia.ð", Line 99999, Column 1)
--             ( DVNE.fromVector
--                 [ Highlight (Just "small label") (DVNE.singleton (Line 99999, Column 1, Column 2)),
--                   Highlight
--                     (Just "overlap")
--                     ( DVNE.singleton (Line 100000, Column 1, Column 4) <> DVNE.singleton (Line 100000, Column 3, Column 7)
--                     )
--                 ]
--             )
--             [ "tiny",
--               "overlap"
--             ]
--         ]
--     )

-- testDiag3 :: Diagnostic
-- testDiag3 =
--   Diagnostic
--     (Just "E0003")
--     Info
--     Nothing
--     Nothing
--     Nothing
--     ( DVNE.fromVector
--         [ Snippet
--             (Nothing, Line 9, Column 1)
--             ( DVNE.fromVector
--                 [ Highlight (Just "overlapping 1") (DVNE.singleton (Line 9, Column 1, Column 4) <> DVNE.singleton (Line 10, Column 1, Column 4)),
--                   Highlight (Just "overlapping 2") (DVNE.singleton (Line 10, Column 1, Column 4) <> DVNE.singleton (Line 11, Column 1, Column 4)),
--                   Highlight (Just "overlapping 3") (DVNE.singleton (Line 9, Column 1, Column 4) <> DVNE.singleton (Line 12, Column 1, Column 4) <> DVNE.singleton (Line 13, Column 1, Column 4))
--                 ]
--             )
--             [ "blah",
--               "blaz",
--               "sofi",
--               "bing",
--               "cute"
--             ]
--         ]
--     )
