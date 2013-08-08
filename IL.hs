module IL ( Entity(..)
          , Module(..)
          , Block(..)
          , Paragraph(..)
          , Span(..)
          , Code(..)
          , Parameter(..)
          , Result(..)
          ) where

-- | Represents any linkable and documentable thing.
newtype Entity = Entity String
    deriving (Eq, Ord, Show)

-- | A module to document (usually a single file).
data Module = Module Entity [Block]
    deriving (Eq, Show)

instance Ord Module where
    compare (Module entA _) (Module entB _) = compare entA entB

-- | A documentation block for an Entity.
data Block = Block
    { entity :: Entity
    , summary :: Paragraph
    , description :: [Paragraph]
    , parameters :: [Parameter]
    , example :: Maybe Code
    , result :: Maybe Result
    } deriving (Eq, Show)

-- | A newline-delimited section of text.
data Paragraph = TextParagraph [Span]
               | CodeBlock Code
               | QuotedText Paragraph
               deriving (Eq, Show)

-- | Represents a portion of text in a documentation string.
data Span = PlainText String
          | EntityLink Entity
          | WebLink String
          | InlineImage String
          | InlineCode Code
          | EmphasizedText Span
          | StrongText Span
          | UnderlinedText Span
          deriving (Eq, Show)

-- | A block or span of code.
newtype Code = Code String
    deriving (Eq, Show)

-- | One of the parameters to a Function, or one of the values in an Enumeration.
data Parameter = Parameter Entity [Span]
    deriving (Eq, Show)

-- | Describes the value that a Function returns to its caller.
newtype Result = Result [Span]
    deriving (Eq, Show)
