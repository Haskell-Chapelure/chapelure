module Chapelure where

import Chapelure.Handler.Colourful (Config, layoutOptions, render)
import Chapelure.Style (putDocText, renderDoc)
import Chapelure.Types
import Data.Text (Text)
import Prettyprinter (layoutPretty)

-- | Helper for displaying a diagnostic to standard out, using default layout options
displayDiagnostic :: Config -> Diagnostic -> IO ()
displayDiagnostic config diagnostic = putDocText $ layoutPretty (layoutOptions config) $ render config diagnostic

-- | Helper for rendering a diagnostic to text, using default layout options
displayDiagnosticAsString :: Config -> Diagnostic -> Text
displayDiagnosticAsString config diagnostic = renderDoc $ layoutPretty (layoutOptions config) $ render config diagnostic
