{-# LANGUAGE PolyKinds, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, DefaultSignatures, TypeOperators, MultiParamTypeClasses #-}

module Data.Model.Queriable (
    Queriable (..)
) where

import GHC.Generics

import Data.Model.Query


class Queriable d where
    query :: (d (Query d) -> a) -> Query d a
    


class Selectable d where
    select :: d (Query d)

    default select :: (Generic1 d, GSelectable (Rep1 d)) => d (Query d)
    select = to1 gselect



class GSelectable f where
    gselect :: f (Query d)


class GPure1 f a where
    gpure1 :: g a -> f g
    

instance GPure1 f a => GPure1 (Rec1 f) a where
    gpure1 x = Rec1 (gpure1 x)

instance GPure1 f a => GPure1 (M1 i s f) a where
    gpure1 x = M1 (gpure1 x)

    
instance (Selector s) => GSelectable (M1 S s a) where
    gselect = M1 (gpure1 (Selected (selName (undefined :: t s f a))))


instance (GSelectable a, GSelectable b) => GSelectable (a :*: b) where
    gselect = gselect :*: gselect


sel :: (Generic1 d, GSelectable (Rep1 d)) => d (Query d)
sel = to1 gselect


-- newtype PK f (p :: k -> *) = PK {unPK :: p (f p)}

-- newtype RecK = RecK {unRecK :: p (f p)}