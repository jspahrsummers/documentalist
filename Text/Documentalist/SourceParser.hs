{-# LANGUAGE ScopedTypeVariables, GADTs, KindSignatures #-}
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

-- | Any kind of documentable declaration.
-- | forall . k
data Declaration :: * -> * where
    DecNode :: k -> Identifier -> DecFNode -> [Declaration k] -> Declaration k
    DecLeaf :: k -> Identifier -> DecFLeaf                    -> Declaration k

instance Show (Declaration t) where
  show (DecNode i n k xs) = "TODO"

data DecFNode = Class SuperTypes
              | Interface SuperTypes
              | Mixin Type
              | Enumeration UnderlyingType
              | Function ResultTypes
              | ClassMethod ResultTypes
              | InstanceMethod ResultTypes
data DecFLeaf = Property UnderlyingType
              | Constant UnderlyingType
              | Parameter UnderlyingType
              | TypeAlias Type

-- | The meaningful body of a comment in the source language.
--
--   This string should be preprocessed to remove markers, like -- in Haskell or // in C.
newtype Comment = Comment String
    deriving Eq

instance Show Comment where
    show (Comment str) = str
-- 
-- instance (Show t) => Show (DeclMap t) where
--     show (DeclMap dm) =
--         let show' :: Declaration -> Maybe t -> String -> String
--             show' decl mt str = str ++ "\n\t" ++ show decl ++ ": " ++ show mt
--         in Map.foldrWithKey show' "{" dm ++ "\n}"
-- 
-- instance Monoid (DeclMap t) where
--     mempty = DeclMap mempty
--     mappend (DeclMap a) (DeclMap b) = DeclMap $ mappend a b

-- | A single module in the source language.
--
--   The declaration list should contain any top-level declarations in order of appearance.
data Module t = Module String [Declaration t]
instance Eq (Module t) where
  (Module t1 _) == (Module t2 _) = t1 == t2

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
