{-# LANGUAGE FlexibleInstances, DataKinds, GADTs, KindSignatures, ExistentialQuantification, QuasiQuotes, TemplateHaskell, StandaloneDeriving, TypeOperators, ImpredicativeTypes, TypeFamilies, ScopedTypeVariables, QuasiQuotes, OverloadedStrings #-}

module RCL where

divideWithAmount_fromEdge :: TypeLC t
                     => Func (RACSignal CGRect)
                     -> Func (RACSignal CGRect)
                     -> NSLayout
                     -> Bind t TTrue (RACSignal CGSize)
                     -> OM (Bind Local TTrue (RACTuple (RACSignal CGSize) (RACSignal CGSize)))
divideWithAmountPaddingEdge d p e x = mkOM
    $ BindTuple (unsafePerformIO fresh) (unsafePerformIO fresh)
    $ FFunctionT [cexp|[$id:x divideWithAmount:$d padding:$p fromEdge:$e]|] (FRACTuple FRACSignal FRACSignal)
