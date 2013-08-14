module Text.Documentalist.SourceParser where

import Data.Map.Strict

-- | An identifier, as it would be written in the source language.
newtype Identifier = Identifier String
    deriving (Eq, Ord, Show)

-- | A type, as it would be declared in the source language.
newtype Type = Type String
    deriving (Eq, Ord, Show)

-- | The meaningful body of a comment in the source language.
--
--   This string should be preprocessed to remove markers, like -- in Haskell or // in C.
newtype Comment = Comment String
    deriving (Eq, Ord, Show)

-- | Represents a list of types (or just a single type) that another type derives from.
type SuperTypes = [Type]

-- | Represents a list of types that will be returned from a function or method.
type ResultTypes = [Type]

-- | Represents the underlying type for a declaration, if the source language supports it and one was provided.
type UnderlyingType = Maybe Type

-- | Maps declarations to language-specific annotations.
type AnnotationMap = Map Declaration [String]

-- | Maps declarations to their documentation comments.
type CommentMap = Map Declaration (Maybe Comment)

-- | Any kind of documentable declaration.
data Declaration = Class Identifier SuperTypes [Declaration]            -- ^ A class declaration.
                 | Interface Identifier SuperTypes [Declaration]        -- ^ An abstract interface, or Objective-C protocol definition.
                 | Mixin Identifier Type [Declaration]                  -- ^ A mixin, or Objective-C category declaration.
                 | Property Identifier UnderlyingType                   -- ^ A property declaration in a class.
                 | Enumeration Identifier UnderlyingType [Declaration]  -- ^ An enumeration.
                 | Constant Identifier UnderlyingType                   -- ^ A constant within an enumeration or class, or outside of any scope.
                 | Function Identifier ResultTypes [Declaration]        -- ^ A function.
                 | ClassMethod Identifier ResultTypes [Declaration]     -- ^ A class method or static member function.
                 | InstanceMethod Identifier ResultTypes [Declaration]  -- ^ An instance method or member function.
                 | Parameter Identifier UnderlyingType                  -- ^ The parameter to a function or method.
                 | TypeAlias Identifier Type                            -- ^ A redeclaration of a type under a different name.
                 deriving (Eq, Ord, Show)

-- | A single module in the source language.
data Module = Module String [Declaration]
    deriving (Eq, Ord, Show)

-- | A package to treat as a single unit for the purposes of documentation generation.
data Package = Package String [Module]
    deriving (Eq, Ord, Show)
