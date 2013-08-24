{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.SourceParser where

import Control.Exception
import Data.List
import qualified Data.Map.Strict as Map
import Data.Monoid
import Text.Documentalist.Util

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

-- | Any kind of documentable declaration, associated with data of type @t@.
data Declaration t = Class t Identifier SuperTypes [Declaration t]              -- ^ A class declaration.
                   | Interface t Identifier SuperTypes [Declaration t]          -- ^ An abstract interface, or Objective-C protocol definition.
                   | Mixin t Identifier Type [Declaration t]                    -- ^ A mixin, or Objective-C category declaration.
                   | Property t Identifier UnderlyingType                       -- ^ A property declaration in a class.
                   | Enumeration t Identifier UnderlyingType [Declaration t]    -- ^ An enumeration.
                   | Constant t Identifier UnderlyingType                       -- ^ A constant within an enumeration or class, or outside of any scope.
                   | Function t Identifier ResultTypes [Declaration t]          -- ^ A function.
                   | ClassMethod t Identifier ResultTypes [Declaration t]       -- ^ A class method or static member function.
                   | InstanceMethod t Identifier ResultTypes [Declaration t]    -- ^ An instance method or member function.
                   | Parameter t Identifier UnderlyingType                      -- ^ The parameter to a function or method.
                   | TypeAlias t Identifier Type                                -- ^ A redeclaration of a type under a different name.
                   deriving (Eq, Show)

-- | The meaningful body of a comment in the source language.
--
--   This string should be preprocessed to remove markers, like -- in Haskell or // in C.
newtype Comment = Comment String
    deriving Eq

instance Show Comment where
    show (Comment str) = "{- " ++ str ++ " -}"

-- | A single module in the source language.
--
--   The declaration list should contain any top-level declarations in order of appearance.
data Module t = Module String [Declaration t]
    deriving Eq

instance Eq t => Ord (Module t) where
    compare (Module a _) (Module b _) = compare a b

instance Show t => Show (Module t) where
    show (Module n decls) = "Module \"" ++ n ++ "\": " ++ showFormattedList decls

-- | A package to treat as a single unit for the purposes of documentation generation.
data Package t = Package String [Module t]
    deriving Eq

instance Eq t => Ord (Package t) where
    compare (Package a _) (Package b _) = compare a b

instance Show t => Show (Package t) where
    show (Package n mods) = "Package \"" ++ n ++ "\": " ++ showFormattedList mods

-- | Represents a unparsed package in a source language.
class SourcePackage p where
    -- | Parses the package into a language-independent form.
    --
    --   Errors may be indicated with `IOException`s.
    parse :: p -> IO (Package (Maybe Comment))
