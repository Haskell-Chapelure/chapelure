module Chapelure.Types where

import Chapelure.Style (DocText)
import Data.Text (Text)
import Data.Text.Display (Display (displayBuilder), ShowInstance (..))
import Data.Vector (Vector)
import Data.Vector.NonEmpty (NonEmptyVector)
import GHC.Generics (Generic)
import Prettyprinter (Pretty)

data Diagnostic = Diagnostic
  { code :: Maybe Text
  -- ^ Unique diagnostic code that be used to look up more information.
  -- Should be globally unique, and documented for easy searching.
  , severity :: Severity
  -- ^ Diagnostic severity, this may be used by the Report Handler
  -- to adapt the formatting of the diagnostic.
  , message :: Maybe DocText
  -- ^ A short description of the diagnostic. Rendered at the top.
  , help :: Maybe DocText
  -- ^ Additional free-form text for the poor bastard at the end of it all. Rendered at the bottom
  , link :: Maybe Text
  -- ^ Link to visit for a more detailed explanation.
  -- Can make use of the 'code' component.
  , snippets :: Maybe (NonEmptyVector Snippet)
  -- ^ Contextual snippet to provide relevant source data.
  }
  deriving stock (Show, Generic)

-- | Enum used to indicate the severity of a diagnostic.
-- The Report Handlers will use this to adapt the formatting of the diagnostic.
data Severity
  = Info
  | Warning
  | Error
  deriving stock (Show, Eq, Enum, Bounded)
  deriving
    (Display)
    via (ShowInstance Severity)

-- | Wrapper to mark an offset from the beginning of a 'Highlight'.
newtype Offset = Offset Word
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord, Enum, Pretty)

-- | Wrapper that represents a line in a Diagnostic report
newtype Line = Line Word
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord, Enum, Pretty)

instance Display Line where
  displayBuilder (Line line) = displayBuilder line

incrementLine :: Line -> Line
incrementLine (Line i) = Line (i + 1)

-- | Wrapper that represents a column in a Diagnostic report
newtype Column = Column Word
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord, Enum, Pretty)

instance Display Column where
  displayBuilder (Column column) = displayBuilder column

incrementColumn :: Column -> Column
incrementColumn (Column i) = Column (i + 1)

-- | A datatype holding a message, some source data and a span to highlight.
data Snippet = Snippet
  { location :: (Maybe Text, Line, Column)
  -- ^ A location name (filename or other), and the line and columns
  -- at which the snippet starts.
  , highlights :: Maybe (NonEmptyVector Highlight)
  -- ^ Highlights of the source that are of interest
  , content :: Vector DocText
  -- ^ Subject that is being reported. Each member is a line.
  }
  deriving stock (Show, Generic)

-- | A piece of source data that is shown when reporting an error.
-- Pointers on a source are always on a single line.
data Highlight = Highlight
  { label :: Maybe DocText
  -- ^ An optional label for the highlight
  , spans :: NonEmptyVector (Line, Column, Column)
  -- ^ Where the highlight is
  }
  deriving stock (Show, Generic)
