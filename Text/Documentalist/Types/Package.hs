{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Text.Documentalist.Types.Package ( Identifier(..)
                                        , Type(..)
                                        , SuperTypes
                                        , ResultTypes
                                        , UnderlyingType
                                        , Declaration(..)
                                        , DecFNode(..)
                                        , DecFLeaf(..)
                                        , Module(..)
                                        , Package(..)
                                        , traversePackage
                                        ) where

import Data.Foldable hiding (mapM_)
import Data.Traversable
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
data Declaration t = DecLeaf t Identifier DecFLeaf
                   | DecNode t Identifier DecFNode [Declaration t]
                   deriving (Eq, Show, Functor, Foldable, Traversable)

-- | A declaration family type for a declaration that may contain more declarations inside of it
data DecFNode = Class           SuperTypes     -- ^ A class declaration.
              | Interface       SuperTypes     -- ^ An abstract interface, or Objective-C protocol definition.
              | Mixin           Type           -- ^ A mixin, or Objective-C category declaration.
              | Enumeration     UnderlyingType -- ^ An enumeration.
              | Function        ResultTypes    -- ^ A function.
              | ClassMethod     ResultTypes    -- ^ A class method or static member function.
              | InstanceMethod  ResultTypes    -- ^ An instance method or member function.
              deriving (Eq, Show)

-- | A declaration family type that can not carry any additional declarations
data DecFLeaf = Property        UnderlyingType -- ^ A property declaration in a class.
              | Constant        UnderlyingType -- ^ A constant within an enumeration or class, or outside of any scope.
              | Parameter       UnderlyingType -- ^ The parameter to a function or method.
              | TypeAlias       Type           -- ^ A redeclaration of a type under a different name.
              deriving (Eq, Show)

-- | A single module in the source language.
--
--   The declaration list should contain any top-level declarations in order of appearance.
data Module t = Module String [Declaration t]
    deriving (Eq, Functor, Foldable, Traversable)

instance Eq t => Ord (Module t) where
    compare (Module a _) (Module b _) = compare a b

instance Show t => Show (Module t) where
    show (Module n decls) = "Module \"" ++ n ++ "\": " ++ showFormattedList decls

-- | A package to treat as a single unit for the purposes of documentation generation.
data Package t = Package String [Module t]
    deriving (Eq, Functor, Foldable, Traversable)

instance Eq t => Ord (Package t) where
    compare (Package a _) (Package b _) = compare a b

instance Show t => Show (Package t) where
    show (Package n mods) = "Package \"" ++ n ++ "\": " ++ showFormattedList mods

-- | Traverse a package with a monad
traversePackage :: Monad m
                => (String -> m ())                 -- ^ An action to perform with the package name.
                -> (String -> m ())                 -- ^ An action to perform with the module name.
                -> (Int -> Declaration t -> m ())   -- ^ An action to perform with each 'Declaration'.
                                                    --   The @Int@ represents the zero-based nesting level.
                -> Package t                        -- ^ The package to traverse.
                -> m ()
traversePackage pkgName modName declFun (Package n ms) = pkgName n >> mapM_ (traverseModule modName declFun) ms

-- | Traverse a module with a monad
traverseModule :: Monad m
               => (String -> m ())                  -- ^ An action to perform with the module name.
               -> (Int -> Declaration t -> m ())    -- ^ An action to perform with each 'Declaration'.
                                                    --   The @Int@ represents the zero-based nesting level.
               -> Module t                          -- ^ The module to traverse.
               -> m ()
traverseModule modName declFun (Module n xs) = modName n >> mapM_ (traverseDeclaration 0 declFun) xs

-- | Traverse a declaration with a monad
traverseDeclaration :: Monad m
                    => Int                              -- ^ The zero-based nesting level that the input 'Declaration' exists at.
                    -> (Int -> Declaration t -> m ())   -- ^ An action to perform with each 'Declaration'.
                                                        --   The @Int@ represents the zero-based nesting level.
                    -> Declaration t                    -- ^ The declaration to traverse.
                    -> m ()
traverseDeclaration d declFun x@(DecLeaf _ _ _)    = declFun d x
traverseDeclaration d declFun x@(DecNode _ _ _ xs) = declFun d x >> mapM_ (traverseDeclaration (d + 1) declFun) xs
