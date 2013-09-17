{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances, OverloadedStrings #-}
-- WARNING: Only use this for testing. The data definitions are missing.
-- GHC 7.8 will make this trivial.

module TestHelpers where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Typeable
import Data.Data
import Text.Documentalist.CommentParser.TomDoc.Parser
import Text.Documentalist.CommentParser
import Text.Documentalist.Types.DocBlock
import Text.Documentalist.Types.Package
import Text.Documentalist.Types.Comment
 
docblock = QuasiQuoter { quoteExp = docblockQ }

-- if you accidentally use a reference, it will break at compile time quoting
-- remember, this is for testing.
docblockQ str = dataToExpQ antiDoc $ parseComment undefined (Comment str)

instance Typeable (Declaration a) where
  -- WARNING: missing implementation
  
deriving instance Typeable DocBlock
deriving instance Typeable Paragraph
deriving instance Typeable DocParam
deriving instance Typeable Code
deriving instance Typeable Result
deriving instance Typeable Span

instance Data (Declaration a) where
  -- WARNING: missing implementation
  
deriving instance Data DocBlock
deriving instance Data Paragraph
deriving instance Data DocParam
deriving instance Data Code
deriving instance Data Result
deriving instance Data Span
deriving instance Data CommentParseException

antiDoc _ = Nothing
