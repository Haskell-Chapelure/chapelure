module Chapelure.Types where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Text as T

data Diagnostic = Diagnostic
  {
    -- | Unique diagnostic code that be used to look up more information. Should be globally unique, and documented for easy searching.
    code :: Maybe Text
    ,
    -- | Diagnostic severity, this may be used by the Report Handler to change the display.
    -- If 'Nothing', the Report Handler should treat it as 'Error'.
    severity :: Maybe Severity
    ,
    -- | Additional free-form text for the poor bastard at the end of it all.
    help :: Maybe Text
    ,
    -- | URL to visit for a more detailed explanation. Can make use of the 'code'.
    url :: Maybe Text
    ,
    -- | Contextual snippet to provide relevant source data.
    snippets :: Maybe (Vector Snippet)
  } deriving (Show, Eq)

-- | Enum used to indicate the severity of a diagnostic. The Report Handlers will use this to
-- alter the formatting of the diagnostic.
data Severity
  = Info
  | Warning
  | Error
  deriving (Show, Eq, Enum, Bounded)

-- | A datatype holding a message, some source data and a span to highlight.
data Snippet = Snippet
  { message :: Maybe Text
  , source :: Source
  , context :: SourceSpan
  }
  deriving (Show, Eq)

-- | A source fragment that can be named.
data Source = Source
  { name :: Maybe Text
  , content :: Text
  }
  deriving (Show, Eq)

-- | Opaque wrapper to mark an offset from the beginning of a 'Source'.
newtype Offset = Offset Word
  deriving (Eq, Show)
  deriving newtype (Num, Ord)

-- | Wrapper that represents a line in a Diagnostic report
newtype Line = Line Word
  deriving (Eq, Show)
  deriving newtype (Num, Ord)

-- | Wrapper that represents a column in a Diagnostic report
newtype Column = Column Word
  deriving (Eq, Show)
  deriving newtype (Num, Ord)

-- | Produce an offset according to source data, and starting line and column.
mkOffset :: Text -> Line -> Column -> Offset
mkOffset source l c = go (T.uncons source) 0 0 0
  where
    go :: Maybe (Char, Text) -> Line -> Column -> Offset -> Offset
    go Nothing _ _ offset = offset
    go (Just ('\n', rest)) line _ offset = go (T.uncons rest) (line + 1) 0 (offset + 1)
    go (Just (_, _)) line col offset | (line + 1) >= l && (col + 1) >= c = offset
    go (Just (_, rest)) line col offset = go (T.uncons rest) line (col + 1) (offset + 1)

data SourceSpan = SourceSpan
  {
    -- | Start of the span from the beginning of a 'Source'
    start  :: Word
  ,
    -- | Total length of the 'SourceSpan' from 'start'
    length :: Word
  ,
    -- | Absolute offset in bytes from the beginning of a 'Source'
    offset :: Word
  ,
    -- | Total length in bytes of the SourceSpan
    size   :: Word
  }
  deriving (Eq, Show)

-- | Contents of a 'Source' covered by a 'SourceSpan'
data SpanContents = SpanContents
  { content :: Vector Text
  , line :: Line
  , column :: Column
  }
  deriving (Eq, Show)
