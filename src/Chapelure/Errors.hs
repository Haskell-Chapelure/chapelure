module Chapelure.Errors where

import Data.Text
import Chapelure.Types
import Data.Text.Display

-- | Error-in-error: Don't interrupt the workflow if a span is out of bounds.
-- Display the diagnostic, and insert a little "info" about the span.
data ChapelureError
  = SpanOutOfBounds
    Text -- ^ Label of the faulty snippet highlight
    (Column, Column) -- Start and end column for the faulty highlight
  deriving stock (Eq, Show)
  deriving Display
    via (ShowInstance ChapelureError)

toDiagnostic :: ChapelureError -> Diagnostic
toDiagnostic (SpanOutOfBounds label (start, end)) =
  Diagnostic{ code = Just "OutOfBoundsError"
            , severity = Error
            , help = Just $ "Bounds errors on snippet for “" <> label <> "”. Check that the 'startColumn' and 'endColumn' bounds " <> display (start, end) <> "are correct"
            , link = Nothing
            , snippets = Nothing
            }
