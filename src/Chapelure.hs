module Chapelure where

import qualified Data.Text.IO as T
import Chapelure.Types
import Data.Text

displayRender :: Diagnostic -- ^ Diagnostic to render
              -> (Diagnostic -> Text) -- ^ Rendering function
              -> IO ()
displayRender diagnostic renderer = T.putStrLn $ renderer diagnostic
