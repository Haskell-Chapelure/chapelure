{-# LANGUAGE QuasiQuotes #-}
module Main where

import Test.Hspec
import Data.String.QQ
import Data.Vector as Vec
import Data.Vector.NonEmpty as NEVec
import Data.Text as T

import Chapelure.Types
import qualified Chapelure.Handler.PlainText as PT

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "PlainText Renderer" $ do
      it "Simple reporting" $ do
        let helpMessage = "Did you check all the types of your arguments?"
        let diagnostic = Diagnostic { code = Just "L342"
                                    , severity = Error
                                    , link = Just "https://localhost:8888/help/code/L342"
                                    , help = Just helpMessage
                                    , snippets = Nothing
                                    }
        let rendered = [s|
Diagnostic severity: Error
 
Help: Did you check all the types of your arguments?
Code: L342
Link: https://localhost:8888/help/code/L342|]
        PT.render diagnostic `shouldBe` rendered

      it "Simple snippet rendering" $ do
        let helpMessage = "Did you check all the types of your arguments?"
        let snip = Snippet { location = ("Code.hs", Line 1, Column 10)
                           , highlights = Nothing
                           , content = Vec.fromList $ T.lines $ T.pack "add :: Int\nadd = 1 + True"
                           }
        let diagnostic = Diagnostic { code = Just "L342"
                                    , severity = Error
                                    , link = Just "https://localhost:8888/help/code/L342"
                                    , help = Just helpMessage
                                    , snippets = Just . NEVec.singleton $ snip
                                    }
        let rendered = [s|
Diagnostic severity: Error
 
-> Code.hs:1:10
1 │ add :: Int
2 │ add = 1 + True

Help: Did you check all the types of your arguments?
Code: L342
Link: https://localhost:8888/help/code/L342|]
        PT.render diagnostic `shouldBe` rendered

      it "Highlighted snippet rendering with one highlight per line" $ do
        let helpMessage = "Did you check all the types of your arguments?"
        let highlights = NEVec.fromList [
                   Source{ label = Just "Return type is an “Int”"
                         , line = Line 3
                         , startColumn = Column 7
                         , endColumn = Column 10
                         }
                 , Source{ label = Just "You tried to pass a “Bool”"
                         , line = Line 4
                         , startColumn = Column 10
                         , endColumn = Column 14
                         }
                ]
        let snip = Snippet { location = ("Code.hs", Line 3, Column 1)
                           , highlights = highlights
                           , content = Vec.fromList $ T.lines $ T.pack "add :: Int\nadd = 1 + True"
                           }
        let diagnostic = Diagnostic { code = Just "L342"
                                    , severity = Error
                                    , link = Just "https://localhost:8888/help/code/L342"
                                    , help = Just helpMessage
                                    , snippets = Just . NEVec.singleton $ snip
                                    }
        let rendered = [s|
Diagnostic severity: Error
 
-> Code.hs:3:1
3 │ add :: Int
           ^^^ Return type is an “Int”
4 │ add = 1 + True
              ^^^^ You tried to pass a “Bool”

Help: Did you check all the types of your arguments?
Code: L342
Link: https://localhost:8888/help/code/L342|]
        PT.render diagnostic `shouldBe` rendered

      it "Highlighted snippet rendering with two highlights on one line" $ do
        let helpMessage = "Did you check all the types of your arguments?"
        let highlights = NEVec.fromList [
                   Source{ label = Just "Return type is “Int”"
                         , line = Line 3
                         , startColumn = Column 7
                         , endColumn = Column 10
                         }
                 , Source{ label = Just "This takes an “Int”"
                         , line = Line 4
                         , startColumn = Column 8
                         , endColumn = Column 8
                         }
                 , Source{ label = Just "But you passed a “Bool”"
                         , line = Line 4
                         , startColumn = Column 10
                         , endColumn = Column 14
                         }
                ]
        let snip = Snippet { location = ("Code.hs", Line 3, Column 1)
                           , highlights = highlights
                           , content = Vec.fromList $ T.lines $ T.pack "add :: Int\nadd = 1 + True"
                           }
        let diagnostic = Diagnostic { code = Just "L342"
                                    , severity = Error
                                    , link = Just "https://localhost:8888/help/code/L342"
                                    , help = Just helpMessage
                                    , snippets = Just . NEVec.singleton $ snip
                                    }
        let rendered = [s|
Diagnostic severity: Error
 
-> Code.hs:3:1
3 │ add :: Int
           ^^^ Return type is “Int”
4 │ add = 1 + True
              ^^^^ But you passed a “Bool”
            ^ This takes an “Int”

Help: Did you check all the types of your arguments?
Code: L342
Link: https://localhost:8888/help/code/L342|]
        PT.render diagnostic `shouldBe` rendered
