{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.SourceParser where

import Control.Exception
import Data.Map.Strict
import Data.Monoid

-- | An identifier, as it would be written in the source language.
newtype Identifier = Identifier String
    deriving (Eq, Ord)

instance Show Identifier where
    show (Identifier str) = "`" ++ str ++ "`"

-- | A type, as it would be declared in the source language.
newtype Type = Type String
    deriving (Eq, Ord)

instance Show Type where
    show (Type str) = "`" ++ str ++ "`"

-- | Represents a list of types (or just a single type) that another type derives from.
type SuperTypes = [Type]

-- | Represents a list of types that will be returned from a function or method.
type ResultTypes = [Type]

-- | Represents the underlying type for a declaration, if the source language supports it and one was provided.
type UnderlyingType = Maybe Type

-- | Maps declarations to language-specific annotations.
type AnnotationMap = Map Declaration [String]

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

-- | The meaningful body of a comment in the source language.
--
--   This string should be preprocessed to remove markers, like -- in Haskell or // in C.
newtype Comment = Comment String
    deriving Eq

instance Show Comment where
    show (Comment str) = str

-- | Maps declarations to optional values of type @t@, which should represent some kind
--   of comment or documentation data.
newtype DeclMap t = DeclMap (Map Declaration (Maybe t))
    deriving Eq

instance (Show t) => Show (DeclMap t) where
    show (DeclMap dm) =
        let show' :: Declaration -> Maybe t -> String -> String
            show' decl mt str = str ++ "\n\t" ++ show decl ++ ": " ++ show mt
        in foldrWithKey show' "{" dm ++ "\n}"

instance Monoid (DeclMap t) where
    mempty = DeclMap mempty
    mappend (DeclMap a) (DeclMap b) = DeclMap $ mappend a b

-- | A single module in the source language.
--
--   The declaration list should contain any top-level declarations in order of appearance.
data Module t = Module String (DeclMap t) [Declaration]
    deriving (Eq, Show)

instance Eq t => Ord (Module t) where
    compare (Module a _ _) (Module b _ _) = compare a b

-- | A package to treat as a single unit for the purposes of documentation generation.
data Package t = Package String [Module t]
    deriving (Eq, Show)

instance Eq t => Ord (Package t) where
    compare (Package a _) (Package b _) = compare a b

-- | Represents a unparsed package in a source language.
class SourcePackage p where
    -- | Parses the package into a language-independent form.
    --
    --   Errors may be indicated with `IOException`s.
    parse :: p -> IO (Package Comment)
