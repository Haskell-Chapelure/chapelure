{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Chapelure.Style where

import Control.Applicative ((<|>))
import Data.Colour (Colour)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Word (Word8)
import Prettyprinter (Doc, SimpleDocStream (..))
import System.Console.ANSI (Color, ColorIntensity, ConsoleIntensity, ConsoleLayer (Background, Foreground), SGR (SetColor, SetConsoleIntensity, SetDefaultColor, SetItalicized, SetPaletteColor, SetRGBColor, SetSwapForegroundBackground, SetUnderlining), Underlining, setSGR)

data StyleNegative = StyleNegative

data Style = Style
  { _intensity :: !(Maybe ConsoleIntensity),
    _italicize :: !(Maybe Bool),
    _underline :: !(Maybe Underlining),
    _negative :: !(Maybe Bool),
    _colorFG :: !(Maybe StyleColor),
    _colorBG :: !(Maybe StyleColor)
  }
  deriving (Show, Eq)

styleIntensity :: ConsoleIntensity -> Style
styleIntensity si = Style (Just si) Nothing Nothing Nothing Nothing Nothing

styleItalicized :: Bool -> Style
styleItalicized it = Style Nothing (Just it) Nothing Nothing Nothing Nothing

styleUnderline :: Underlining -> Style
styleUnderline ul = Style Nothing Nothing (Just ul) Nothing Nothing Nothing

styleNegative :: Bool -> Style
styleNegative sn = Style Nothing Nothing Nothing (Just sn) Nothing Nothing

styleFG :: StyleColor -> Style
styleFG c = Style Nothing Nothing Nothing Nothing (Just c) Nothing

styleBG :: StyleColor -> Style
styleBG c = Style Nothing Nothing Nothing Nothing Nothing (Just c)

instance Semigroup Style where
  Style is it un ne fg bg <> Style is2 it2 un2 ne2 fg2 bg2 =
    Style (is <|> is2) (it <|> it2) (un <|> un2) (ne <|> ne2) (fg <|> fg2) (bg <|> bg2)

instance Monoid Style where
  mempty = Style Nothing Nothing Nothing Nothing Nothing Nothing

data StyleColor
  = ColorDefault
  | Color16 !ColorIntensity !Color
  | Color256 !Word8
  | ColorRGB !(Colour Float)
  deriving (Show, Eq)

type DocText = Doc Style

setStyle :: Style -> IO ()
setStyle (Style is it un ne fg bg) =
  setSGR $
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

putDocText :: SimpleDocStream Style -> IO ()
putDocText = go []
  where
    go :: [Style] -> SimpleDocStream Style -> IO ()
    go st = \case
      SFail -> error "SFail left in pretty-printed output"
      SEmpty -> pure ()
      SChar c sds -> putChar c >> go st sds
      SText _n txt sds -> T.putStr txt >> go st sds
      SLine n sds -> putChar '\n' >> T.putStr (T.replicate n (T.singleton ' ')) >> go st sds
      SAnnPush st' sds -> case st of
        [] -> do
          setStyle st'
          go [st'] sds
        (s : ss) -> do
          setStyle (s <> st')
          go (st' : s : ss) sds
      SAnnPop sds -> case st of
        [] -> go [] sds
        [_s] -> setStyle mempty >> go [] sds
        (_s : t : ss) -> setStyle t >> go (t : ss) sds