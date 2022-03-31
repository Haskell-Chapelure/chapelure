{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Utilities for styling text for terminal output
module Chapelure.Style where

import Control.Applicative ((<|>))
import Data.Colour (Colour)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Word (Word8)
import Prettyprinter (Doc, SimpleDocStream (..))
import System.Console.ANSI (Color, ColorIntensity, ConsoleIntensity, ConsoleLayer (Background, Foreground), SGR (SetColor, SetConsoleIntensity, SetDefaultColor, SetItalicized, SetPaletteColor, SetRGBColor, SetSwapForegroundBackground, SetUnderlining), Underlining, hSetSGR, setSGR, setSGRCode)
import System.IO (Handle)

-- | A collection of style information. Implements 'Monoid'
data Style = Style
  { _intensity :: !(Maybe ConsoleIntensity),
    _italicize :: !(Maybe Bool),
    _underline :: !(Maybe Underlining),
    _negative :: !(Maybe Bool),
    _colorFG :: !(Maybe StyleColor),
    _colorBG :: !(Maybe StyleColor)
  }
  deriving (Show, Eq)

-- | Set the intensity of a style
styleIntensity :: ConsoleIntensity -> Style
styleIntensity si = Style (Just si) Nothing Nothing Nothing Nothing Nothing

-- | Set whether or not a style is italicized
styleItalicized :: Bool -> Style
styleItalicized it = Style Nothing (Just it) Nothing Nothing Nothing Nothing

-- | Set the type of underling for a style
styleUnderline :: Underlining -> Style
styleUnderline ul = Style Nothing Nothing (Just ul) Nothing Nothing Nothing

-- | Sets negative mode for a style; this inverts the foreground and background colours
styleNegative :: Bool -> Style
styleNegative sn = Style Nothing Nothing Nothing (Just sn) Nothing Nothing

-- | Set the foreground color of a style
styleFG :: StyleColor -> Style
styleFG c = Style Nothing Nothing Nothing Nothing (Just c) Nothing

-- | Set the background color of a style
styleBG :: StyleColor -> Style
styleBG c = Style Nothing Nothing Nothing Nothing Nothing (Just c)

instance Semigroup Style where
  Style is it un ne fg bg <> Style is2 it2 un2 ne2 fg2 bg2 =
    Style (is <|> is2) (it <|> it2) (un <|> un2) (ne <|> ne2) (fg <|> fg2) (bg <|> bg2)

instance Monoid Style where
  mempty = Style Nothing Nothing Nothing Nothing Nothing Nothing

-- | Various kinds of colors that can be used in a terminal
data StyleColor
  = ColorDefault
  -- | A 16-colour palette: 8 hues and 2 intensities. The most commonly supported form of terminal colouring
  | Color16 !ColorIntensity !Color
  -- | A fixed 256-color palette
  | Color256 !Word8
  -- | Full 24-bit true colors
  | ColorRGB !(Colour Float)
  deriving (Show, Eq)

-- | Styled, pretty-printed 'Doc'uments
type DocText = Doc Style

-- | Converts a 'Style' to a list of Set Graphics Rendition 'SGR' codes
toSGR :: Style -> [SGR]
toSGR (Style is it un ne fg bg) =
  catMaybes
    [ SetConsoleIntensity <$> is,
      SetItalicized <$> it,
      SetUnderlining <$> un,
      SetSwapForegroundBackground <$> ne,
      go Foreground <$> fg,
      go Background <$> bg
    ]
  where
    go :: ConsoleLayer -> StyleColor -> SGR
    go l = \case
      ColorDefault -> SetDefaultColor l
      Color16 ci co -> SetColor l ci co
      Color256 wo -> SetPaletteColor l wo
      ColorRGB co -> SetRGBColor l co

-- | Converts a style to a string containing the escape codes to set that style
setStyleCode :: Style -> String
setStyleCode = setSGRCode . toSGR

-- | Sets the graphics rendition mode of standard out to a style
setStyle :: Style -> IO ()
setStyle = setSGR . toSGR

-- | Sets the graphics rendition mode of a handle to a style
hSetStyle :: Handle -> Style -> IO ()
hSetStyle h = hSetSGR h . toSGR

-- | Render a styled document stream to text, including ANSI escape codes to set the style
renderDoc :: SimpleDocStream Style -> T.Text
renderDoc = go []
  where
    go :: [Maybe Style] -> SimpleDocStream Style -> T.Text
    go st = \case
      SFail -> error "SFail left in pretty-printed output"
      SEmpty -> mempty
      SChar c sds -> T.singleton c <> go st sds
      SText _n txt sds -> txt <> go st sds
      SLine n sds -> T.singleton '\n' <> T.replicate n (T.singleton ' ') <> go st sds
      SAnnPush st' sds -> if st' == mempty
        then go (Nothing : st) sds
        else T.pack (setStyleCode (fromMaybe mempty (fromMaybe mempty (listToMaybe st)) <> st')) <> go (Just st' : st) sds
      SAnnPop sds -> case st of
        [] -> go [] sds
        [Nothing] -> go [] sds
        (Nothing : t : ss) -> go (t : ss) sds
        [Just _s] -> T.pack (setStyleCode mempty) <> go [] sds
        (Just _s : t : ss) -> maybe mempty (T.pack . setStyleCode) t <> go (t : ss) sds

-- | Output a styled document stream to standard out
putDocText :: SimpleDocStream Style -> IO ()
putDocText = go []
  where
    go :: [Maybe Style] -> SimpleDocStream Style -> IO ()
    go st = \case
      SFail -> error "SFail left in pretty-printed output"
      SEmpty -> pure ()
      SChar c sds -> putChar c >> go st sds
      SText _n txt sds -> T.putStr txt >> go st sds
      SLine n sds -> putChar '\n' >> T.putStr (T.replicate n (T.singleton ' ')) >> go st sds
      SAnnPush st' sds -> if st' == mempty
        then go (Nothing : st) sds
        else do
          setStyle (fromMaybe mempty (fromMaybe mempty (listToMaybe st)) <> st')
          go (Just st' : st) sds
      SAnnPop sds -> case st of
        [] -> go [] sds
        [Nothing] -> go [] sds
        (Nothing : t : ss) -> go (t : ss) sds
        [Just _s] -> setStyle mempty >> go [] sds
        (Just _s : t : ss) -> maybe (pure ()) setStyle t >> go (t : ss) sds