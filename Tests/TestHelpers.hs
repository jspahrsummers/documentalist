{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}
module Tests.TestHelpers where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Typeable
import Data.Data
import Text.Documentalist.CommentParser.TomDoc.Parser
import Text.Documentalist.Types.DocBlock
import Text.Documentalist.Types.Package
 
docblock = QuasiQuoter { quoteExp = docblockQ }

-- if you accidentally use a reference, it will break at compile time quoting
-- remember, this is for testing.
docblockQ str = dataToExpQ antiDoc $ parseComment undefined str

instance Typeable (Declaration a) where
  
deriving instance Typeable DocBlock
deriving instance Typeable Paragraph
deriving instance Typeable DocParam
deriving instance Typeable Code
deriving instance Typeable Result
deriving instance Typeable Span

instance Data (Declaration a) where
  
deriving instance Data DocBlock
deriving instance Data Paragraph
deriving instance Data DocParam
deriving instance Data Code
deriving instance Data Result
deriving instance Data Span

antiDoc _ = Nothing
