module IL ( Entity(..)
          , Example(..)
          , Declaration(..)
          , Parameter(..)
          , Result(..)
          ) where

-- | Represents any linkable and documentable thing.
data Entity = Entity String [Entity]
            deriving (Eq, Show)

-- | A code sample in the source language.
newtype Example = Example String
                deriving (Eq, Show)

-- | Any kind of declaration.
data Declaration = Function Entity [Parameter] (Maybe Example) (Maybe Result)
                 | Enumeration Entity [Parameter]
                 deriving (Eq, Show)

-- | One of the parameters to a Function, or one of the values in an Enumeration.
data Parameter = Parameter Entity String
               deriving (Eq, Show)

-- | Describes the value that a Function returns to its caller.
newtype Result = Result String
               deriving (Eq, Show)
