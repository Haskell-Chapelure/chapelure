{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.String.QQ
import Data.Text as T
import Data.Vector as Vec
import Data.Vector.NonEmpty as NEVec
import Prettyprinter (pretty)
import Test.Hspec

import Chapelure
import qualified Chapelure.Handler.Colourful as PT
import Chapelure.Types

main :: IO ()
main = hspec spec

testForScreenshot :: IO ()
testForScreenshot =
  let helpMessage = "Did you check all the types of your arguments?"
   in let highlights =
            NEVec.fromList
              [ Highlight
                  { label = Just "Return type is “Int”"
                  , spans = NEVec.singleton (Line 3, Column 8, Column 10)
                  }
              , Highlight
                  { label = Just "This takes an “Int”"
                  , spans = NEVec.singleton (Line 4, Column 9, Column 9)
                  }
              , Highlight
                  { label = Just "But you passed a “Bool”"
                  , spans = NEVec.singleton (Line 4, Column 11, Column 14)
                  }
              ]
       in let snip =
                Snippet
                  { location = (Just "Code.hs", Line 3, Column 1)
                  , highlights = highlights
                  , content = Vec.fromList $ fmap pretty $ T.lines $ T.pack "add :: Int\nadd = 1 + True"
                  }
           in let diagnostic =
                    Diagnostic
                      { code = Just "L342"
                      , severity = Error
                      , link = Just "https://localhost:8888/help/code/L342"
                      , message = Nothing
                      , help = Just helpMessage
                      , snippets = Just . NEVec.singleton $ snip
                      }
               in displayDiagnostic PT.prettyConfig diagnostic

spec :: Spec
spec = do
  describe "PlainText Renderer" $ do
    it "Simple reporting" $ do
      let helpMessage = "Did you check all the types of your arguments?"
      let diagnostic =
            Diagnostic
              { code = Just "L342"
              , severity = Error
              , link = Just "https://localhost:8888/help/code/L342"
              , message = Nothing
              , help = Just helpMessage
              , snippets = Nothing
              }
      let rendered =
            [s|
[L342]: Error:
Did you check all the types of your arguments?
https://localhost:8888/help/code/L342
|]
      displayDiagnosticAsString PT.asciiPlainConfig diagnostic `shouldBe` rendered

    it "Simple snippet rendering" $ do
      let helpMessage = "Did you check all the types of your arguments?"
      let snip =
            Snippet
              { location = (Just "Code.hs", Line 1, Column 10)
              , highlights = Nothing
              , content = Vec.fromList $ fmap pretty $ T.lines $ T.pack "add :: Int\nadd = 1 + True"
              }
      let diagnostic =
            Diagnostic
              { code = Just "L342"
              , severity = Error
              , link = Just "https://localhost:8888/help/code/L342"
              , message = Nothing
              , help = Just helpMessage
              , snippets = Just . NEVec.singleton $ snip
              }
      let rendered =
            [s|
[L342]: Error:
 /[Code.hs:1:10] 
 | 
1| add :: Int
2| add = 1 + True
 | Did you check all the types of your arguments?
 | https://localhost:8888/help/code/L342
 / 
|]
      displayDiagnosticAsString PT.asciiPlainConfig diagnostic `shouldBe` rendered

    it "Highlighted snippet rendering with one highlight per line" $ do
      let helpMessage = "Did you check all the types of your arguments?"
      let highlights =
            NEVec.fromList
              [ Highlight
                  { label = Just "Return type is an “Int”"
                  , spans = NEVec.singleton (Line 3, Column 8, Column 10)
                  }
              , Highlight
                  { label = Just "You tried to pass a “Bool”"
                  , spans = NEVec.singleton (Line 4, Column 11, Column 14)
                  }
              ]
      let snip =
            Snippet
              { location = (Just "Code.hs", Line 3, Column 1)
              , highlights = highlights
              , content = Vec.fromList $ fmap pretty $ T.lines $ T.pack "add :: Int\nadd = 1 + True"
              }
      let diagnostic =
            Diagnostic
              { code = Just "L342"
              , severity = Error
              , link = Just "https://localhost:8888/help/code/L342"
              , message = Nothing
              , help = Just helpMessage
              , snippets = Just . NEVec.singleton $ snip
              }
      let rendered =
            [s|
[L342]: Error:
 /[Code.hs:3:1] 
 | 
3| add :: Int
 |        ^^^
 |         \ Return type is an “Int”
4| add = 1 + True
 |           ^^^^
 |            \ You tried to pass a “Bool”
 | Did you check all the types of your arguments?
 | https://localhost:8888/help/code/L342
 / 
|]
      displayDiagnosticAsString PT.asciiPlainConfig diagnostic `shouldBe` rendered

    it "Highlighted snippet rendering with two highlights on one line" $ do
      let helpMessage = "Did you check all the types of your arguments?"
      let highlights =
            NEVec.fromList
              [ Highlight
                  { label = Just "Return type is “Int”"
                  , spans = NEVec.singleton (Line 3, Column 8, Column 10)
                  }
              , Highlight
                  { label = Just "This takes an “Int”"
                  , spans = NEVec.singleton (Line 4, Column 9, Column 9)
                  }
              , Highlight
                  { label = Just "But you passed a “Bool”"
                  , spans = NEVec.singleton (Line 4, Column 11, Column 14)
                  }
              ]
      let snip =
            Snippet
              { location = (Just "Code.hs", Line 3, Column 1)
              , highlights = highlights
              , content = Vec.fromList $ fmap pretty $ T.lines $ T.pack "add :: Int\nadd = 1 + True"
              }
      let diagnostic =
            Diagnostic
              { code = Just "L342"
              , severity = Error
              , link = Just "https://localhost:8888/help/code/L342"
              , message = Nothing
              , help = Just helpMessage
              , snippets = Just . NEVec.singleton $ snip
              }
      let rendered =
            [s|
[L342]: Error:
 /[Code.hs:3:1] 
 | 
3| add :: Int
 |        ^^^
 |         \ Return type is “Int”
4| add = 1 + True
 |         ^
 |         \ This takes an “Int”
 |           ^^^^
 |            \ But you passed a “Bool”
 | Did you check all the types of your arguments?
 | https://localhost:8888/help/code/L342
 / 
|]
      displayDiagnosticAsString PT.asciiPlainConfig diagnostic `shouldBe` rendered
