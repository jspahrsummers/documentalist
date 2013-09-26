{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
module Text.Documentalist.SourceParser.Haskell ( HaskellParser(..)
                                               , (~>)
                                               , parseHS
                                               ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Text.Documentalist.SourceParser as S
import Language.Haskell.Exts as H
import qualified Data.Map as M
import Control.Monad.Writer
import Data.List
import Data.Functor.Identity
import Text.Documentalist.CommentParser -- for unification

newtype HaskellParser p = HaskellParser { runHaskellParser :: IO p }

data ForeignTypeInfo = ForeignTypeInfo [String] deriving Show

instance Monad HaskellParser where
    return x = HaskellParser $ return x
    (HaskellParser m) >>= f =
        HaskellParser $ m >>= return . f >>= runHaskellParser

instance Functor HaskellParser where
    fmap = liftM

instance MonadIO HaskellParser where
    liftIO = HaskellParser

parseHS :: MonadIO m => FilePath -> m (Package ForeignTypeInfo)
parseHS path =
    liftIO $ do
        pr <- parseFile path
        return $ Package "" [genDocs path (fromParseResult pr)]

genDocs :: FilePath -> H.Module -> S.Module ForeignTypeInfo
genDocs file c = S.Module file $ mapMaybe (parseDecl' file) (descendantDecls file c)

-- | Unify a package of Haskell declarations into any other declarations.
-- | The right side is preserved.
-- | NOTE: The left side are the Haskell declarations
(~>) :: Package ForeignTypeInfo -> Package (Either CommentParseException DocBlock) -> Package (Either CommentParseException DocBlock)
a ~> b = let
      lhs = snd $ runWriter $ traversePackage (\_ -> tell []) (\_ -> tell []) extractInformation a
      -- extractInformation :: Int -> Declaration a -> [(String, Declaration a)]
      extractInformation :: Int
                         -> Declaration ForeignTypeInfo
                         -> WriterT [(String, ForeignTypeInfo)] Identity ()
      extractInformation _ (DecNode x (Identifier s) (Function _) _) = tell [(objcify s, x)]
      
      findInformation :: String
                      -> Declaration (Either CommentParseException DocBlock)
                      -> Declaration (Either CommentParseException DocBlock)
      findInformation s x = case M.lookup s (M.fromList lhs) of
        (Just v) -> addInformation v x
        Nothing -> x
      -- turn some haskell function name into it's objc like name
      -- divideWithAmount_fromEdge -> -divideWithAmount:fromEdge
      objcify s = '-' : map (\x -> if x == '_' then ':' else x) s ++ ":"
      -- add some extra v to x
      -- only type information available
      addInformation :: ForeignTypeInfo
                     -> Declaration (Either CommentParseException DocBlock)
                     -> Declaration (Either CommentParseException DocBlock)
      addInformation fti x@(DecNode (Right docblock) i f xs)
          = DecNode (addFuns docblock fti) i f xs
      addInformation _ x = x
      addFuns :: DocBlock -> ForeignTypeInfo -> Either CommentParseException DocBlock
      -- addFuns docblock decs = Right (S.Comment (show decs))
      addFuns docblock (ForeignTypeInfo decs) = Right $ docblock {description = (description docblock) ++ extra}
        where
          extra = [CodeBlock (Code (intercalate ", " decs))]
      idOf (DecLeaf _ (Identifier i) _) = i
    in mapWithNames findInformation b

mapWithNames :: (String -> Declaration a -> Declaration a) -> Package a -> Package a
mapWithNames f (Package n xs) = Package n (map map' xs)
    where
      map' (S.Module s ds) = S.Module s (map map'' ds)
      map'' x@(DecLeaf _ (Identifier s) _) = f s x
      map'' x@(DecNode q (Identifier s) r xs) = f s (DecNode q (Identifier s) r (map map'' xs))

parseDecl' :: FilePath -> H.Decl -> Maybe (Declaration ForeignTypeInfo)
parseDecl' file (TypeSig _ ((Ident n):_) xs) = Just
    $ DecNode (mkParam' xs) (Identifier n) (Function [S.Type (retType xs)]) []
parseDecl' file x = Nothing

retType :: H.Type -> String
retType x = last $ mkParam x

mkParam' :: H.Type -> ForeignTypeInfo
mkParam' xs = ForeignTypeInfo (mkParam xs)

mkParam :: H.Type -> [String]
mkParam (TyForall _ _ t) = mkParam t
mkParam (TyFun t ts) = concat [mkParam t, mkParam ts]
mkParam (TyApp t1 t2) = concat [mkParam t1, mkParam t2]
mkParam (TyTuple _ ts) = concat $ map mkParam ts
mkParam (TyList t) = mkParam t
mkParam (TyParen t) = mkParam t
mkParam (TyInfix t1 _ t2) = concat [mkParam t1, mkParam t2]
mkParam (TyKind t _) = mkParam t
mkParam (TyVar n) = [mkName n]
mkParam (TyCon n) = [mkQName n]

mkQName :: QName -> String
mkQName (UnQual (Ident n)) = n
mkQName (Qual _ (Ident n)) = n
mkQName (Special x) = show x

mkName :: Name -> String
mkName (Ident s) = s
mkName (Symbol s) = s

descendantDecls :: FilePath -> H.Module -> [H.Decl]
descendantDecls file x@(H.Module _ _ n _ _ ins xs) = xs -- TODO
