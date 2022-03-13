module Chapelure where

import Chapelure.Handler.Colourful (Config, layoutOptions, render)
import Chapelure.Style (putDocText)
import Chapelure.Types
import Prettyprinter (layoutPretty)

displayDiagnostic :: Config -> Diagnostic -> IO ()
displayDiagnostic config diagnostic = putDocText $ layoutPretty (layoutOptions config) $ render config diagnostic
