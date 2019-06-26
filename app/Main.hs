{-# LANGUAGE StandaloneDeriving, DefaultSignatures, FlexibleContexts, FlexibleInstances, DeriveGeneric, ScopedTypeVariables, PolyKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where


import GHC.Generics

import Data.Model.Queriable

-- Some data types

data Value = Value

deriving instance Generic Value

instance Described (Value)

-- Some functors

data Constant a = Constant

data Identity a = Identity a

data Twice a = Twice a a

data Glitter a = Shiny a | Sparkly a

deriving instance Generic (Constant a)
deriving instance Generic (Identity a)
deriving instance Generic (Twice a)
deriving instance Generic (Glitter a)

deriving instance Generic1 Constant
deriving instance Generic1 Identity
deriving instance Generic1 Twice
deriving instance Generic1 Glitter

instance Described1 Constant
instance Described1 Identity
instance Described1 Maybe
instance Described1 Twice
instance Described1 Glitter

-- Data types that allow recursion to be controlled

data Parent f = Parent {
    child :: f (Child f)
}

data Child f = Child {
    sum :: f (Sum f),
    product :: f (Product f),
    value :: f Value
}

data Sum f = Left (f Value) | Right (f Value)

data Product f = Product (f Value) (f Value)

deriving instance (Generic1 f) => Generic (Parent f)
deriving instance (Generic1 f) => Generic (Child f)
deriving instance (Generic1 f) => Generic (Sum f)
deriving instance (Generic1 f) => Generic (Product f)


instance (Generic1 f, Described1 f) => Described (Parent f)
instance (Generic1 f, Described1 f) => Described (Child f)
instance (Generic1 f, Described1 f) => Described (Sum f)
instance (Generic1 f, Described1 f) => Described (Product f)


-- Described class

class Described a where
    describe :: a -> String

    default describe :: (Generic a, GDescribed (Rep a)) => a -> String
    describe (_ :: a) = gdescribe (undefined :: Rep a ())


class Described1 f where
    describe1 :: (Described a) => f a -> String

    default describe1 :: (Generic1 f, GDescribed (Rep (f a)), Described a) => f a -> String
    describe1 (_ :: f a) = gdescribe (undefined :: Rep (f a) ())

class DescribedK f where
    describeK :: p f -> String

    default describeK :: (Generic1 f, GDescribed (Rep1 f)) => p f -> String
    describeK (_ :: p f) = gdescribe (undefined :: Rep1 f ())

instance (Described1 f, Described a) => Described (f a) where
    describe = describe1


-- Generic described class

class GDescribed f where
    gdescribe :: f () -> String

instance GDescribed U1 where
    gdescribe _ = "U1"

instance (GDescribed a, GDescribed b) => GDescribed ((:*:) a  b) where
    gdescribe _ = "(" ++ gdescribe (undefined :: a ()) ++ " :*: " ++ gdescribe (undefined :: b ()) ++ ")"


instance (GDescribed a, GDescribed b) => GDescribed ((:+:) a  b) where
    gdescribe _ = "(" ++ gdescribe (undefined :: a ()) ++ " :+: " ++ gdescribe (undefined :: b ()) ++ ")"


instance (GDescribed a, Datatype d) => GDescribed (M1 D d a) where
    gdescribe d = "(M1 D " ++ datatypeName d ++ " " ++ gdescribe (undefined :: a ()) ++ ")"

instance (GDescribed a, Constructor c) => GDescribed (M1 C c a) where
    gdescribe c = "(M1 C " ++ conName c ++ " " ++ gdescribe (undefined :: a ()) ++ ")"

instance (GDescribed a, Selector s) => GDescribed (M1 S s a) where
    gdescribe s = "(M1 S " ++ selName s ++ " " ++ gdescribe (undefined :: a ()) ++ ")"

instance (Described a) => GDescribed (K1 i a) where
    gdescribe _ = "(K1 " ++ describe (undefined :: a) ++ ")"

instance GDescribed Par1 where
    gdescribe _ = "Par1"

instance GDescribed (Rec1 f) where
    gdescribe _ = "Rec1"


-- Run example
main = do
    --putStrLn . describeK $ (undefined :: p (Parent Identity))
    putStrLn . describe $ (undefined :: Parent Constant)
    putStrLn . describe $ (undefined :: Parent Identity)
    putStrLn . describe $ (undefined :: Parent Maybe)
    putStrLn . describe $ (undefined :: Parent Twice)
    putStrLn . describe $ (undefined :: Maybe (Sum Identity))
    putStrLn . describe $ (undefined :: Parent Glitter)
