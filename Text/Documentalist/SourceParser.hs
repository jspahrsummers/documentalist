{-# LANGUAGE ScopedTypeVariables #-}
module Text.Documentalist.SourceParser where

import Text.Documentalist.Util
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Control.Applicative

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
data Declaration t = DecLeaf t Identifier DecFLeaf
                   | DecNode t Identifier DecFNode [Declaration t]
                     deriving (Eq, Show)

data DecFNode = Class           SuperTypes     -- ^ A class declaration.
              | Interface       SuperTypes     -- ^ An abstract interface, or Objective-C protocol definition.
              | Mixin           Type           -- ^ A mixin, or Objective-C category declaration.
              | Enumeration     UnderlyingType -- ^ An enumeration.                   
              | Function        ResultTypes    -- ^ A function.
              | ClassMethod     ResultTypes    -- ^ A class method or static member function.
              | InstanceMethod  ResultTypes    -- ^ An instance method or member function.
                deriving (Eq, Show)

data DecFLeaf = Property        UnderlyingType -- ^ A property declaration in a class.
              | Constant        UnderlyingType -- ^ A constant within an enumeration or class, or outside of any scope.
              | Parameter       UnderlyingType -- ^ The parameter to a function or method.
              | TypeAlias       Type           -- ^ A redeclaration of a type under a different name.
                deriving (Eq, Show)

instance Functor Declaration where
    fmap f (DecLeaf t i d)    = DecLeaf (f t) i d
    fmap f (DecNode t i d xs) = DecNode (f t) i d (map (fmap f) xs)

instance F.Foldable Declaration where
    foldr f z (DecLeaf t i d)    = f t z
    foldr f z (DecNode t i d xs) = Prelude.foldr (flip (F.foldr f)) (f t z) xs

instance T.Traversable Declaration where
    traverse f (DecLeaf t i d)    = DecLeaf <$> f t <*> pure i <*> pure d
    traverse f (DecNode t i d xs) = DecNode <$> f t <*> pure i <*> pure d <*> T.traverse (T.traverse f) xs

-- | The meaningful body of a comment in the source language.
--
--   This string should be preprocessed to remove markers, like -- in Haskell or // in C.
newtype Comment = Comment String
    deriving Eq

instance Show Comment where
    show (Comment str) = "\n{- " ++ str ++ " -}"

-- | A single module in the source language.
--
--   The declaration list should contain any top-level declarations in order of appearance.
data Module t = Module String [Declaration t]
    deriving Eq

instance Eq t => Ord (Module t) where
    compare (Module a _) (Module b _) = compare a b

instance Show t => Show (Module t) where
    show (Module n decls) = "Module \"" ++ n ++ "\": " ++ showFormattedList decls

instance Functor Module where
    fmap f (Module s xs) = Module s $ map (fmap f) xs

instance F.Foldable Module where
    foldr f z (Module s xs) = Prelude.foldr (flip (F.foldr f)) z xs

instance T.Traversable Module where
    traverse f (Module s xs) = Module <$> pure s <*> T.traverse (T.traverse f) xs

-- | A package to treat as a single unit for the purposes of documentation generation.
data Package t = Package String [Module t]
    deriving Eq

instance Eq t => Ord (Package t) where
    compare (Package a _) (Package b _) = compare a b

instance Show t => Show (Package t) where
    show (Package n mods) = "Package \"" ++ n ++ "\": " ++ showFormattedList mods

instance Functor Package where
    fmap f (Package s ms) = Package s $ map (fmap f) ms

instance F.Foldable Package where
    foldr f z (Package s ms) = Prelude.foldr (flip (F.foldr f)) z ms

instance T.Traversable Package where
    traverse f (Package s ms) = Package <$> pure s <*> T.traverse (T.traverse f) ms

-- | Represents a unparsed package in a source language.
class SourcePackage p where
    -- | Parses the package into a language-independent form.
    --
    --   Errors may be indicated with `IOException`s.
    parse :: p -> IO (Package (Maybe Comment))
