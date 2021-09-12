module Chapelure.Types where

import Data.Text (Text)
import Data.Vector.NonEmpty (NonEmptyVector)
import GHC.Generics
import Data.Text.Display
import Data.Vector (Vector)
import Prettyprinter (Pretty)

data Diagnostic = Diagnostic
  {
    -- | Unique diagnostic code that be used to look up more information.
    -- Should be globally unique, and documented for easy searching.
    code     :: Maybe Text
    -- | Diagnostic severity, this may be used by the Report Handler
    -- to adapt the formatting of the diagnostic.
  , severity :: Severity
    -- | Additional free-form text for the poor bastard at the end of it all.
  , help     :: Maybe Text
    -- | Link to visit for a more detailed explanation.
    -- Can make use of the 'code' component.
  , link     :: Maybe Text
    -- | Contextual snippet to provide relevant source data.
  , snippets :: Maybe (NonEmptyVector Snippet)
  } deriving stock (Show, Eq, Generic)

-- | Enum used to indicate the severity of a diagnostic.
-- The Report Handlers will use this to adapt the formatting of the diagnostic.
data Severity
  = Info
  | Warning
  | Error
  deriving stock (Show, Eq, Enum, Bounded)
  deriving Display
    via (ShowInstance Severity)

-- | Wrapper to mark an offset from the beginning of a 'Source'.
newtype Offset = Offset Word
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord, Enum, Pretty)

-- | Wrapper that represents a line in a Diagnostic report
newtype Line = Line Word
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord, Pretty)

instance Display Line where
  displayBuilder (Line line) = displayBuilder line

incrementLine :: Line -> Line
incrementLine (Line i) = Line (i + 1)

-- | Wrapper that represents a column in a Diagnostic report
newtype Column = Column Word
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord, Pretty)

instance Display Column where
  displayBuilder (Column column) = displayBuilder column

incrementColumn :: Column -> Column
incrementColumn (Column i) = Column (i + 1)

-- | A datatype holding a message, some source data and a span to highlight.
data Snippet = Snippet
  {
    -- | A location name (filename or other), and the line and columns
    -- at which the snippet starts.
    location :: (Text, Line, Column)
    -- | Highlights of the source that are of interest
  , highlights :: Maybe (NonEmptyVector Source)
    -- | Subject that is being reported. Each member is a line.
  , content :: Vector Text
  }
  deriving stock (Show, Eq, Generic)

-- | A piece of source data that is shown when reporting an error.
-- Pointers on a source are always on a single line.
data Source = Source
  {
    -- | An optional label for the highlight
    label       :: Maybe Text
    -- | The 'Line' on which is the highlight.
  , line        :: Line
    -- | The 'Column' on which starts the highlight.
  , startColumn :: Column
    -- | The 'Column' on which ends the highlight.
  , endColumn   :: Column
  }
  deriving stock (Show, Eq, Generic)
