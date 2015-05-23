{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module UnboxMaybe(DefaultValue(..)) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Base as B
import qualified Data.Vector.Generic.Mutable as M
import Control.Monad (liftM)

class DefaultValue a where
    defaultValue :: a

instance {-# OVERLAPPABLE #-} DefaultValue Bool where
    defaultValue = False


newtype instance U.MVector s (Maybe a) = MV_Maybe (U.MVector s (Bool,a))
newtype instance U.Vector    (Maybe a) = V_Maybe  (U.Vector    (Bool,a))

instance (U.Unbox a, DefaultValue a) => M.MVector U.MVector (Maybe a) where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_Maybe v) = M.basicLength v
    basicUnsafeSlice i n (MV_Maybe v) = MV_Maybe $ M.basicUnsafeSlice i n v
    basicOverlaps (MV_Maybe v1) (MV_Maybe v2) = M.basicOverlaps v1 v2
    basicUnsafeNew n = MV_Maybe `liftM` M.basicUnsafeNew n
    basicUnsafeReplicate n maybeV = MV_Maybe `liftM` M.basicUnsafeReplicate n (fromMaybe maybeV)
    basicUnsafeRead (MV_Maybe v) i = toMaybe `liftM` M.basicUnsafeRead v i
    basicUnsafeWrite (MV_Maybe v) i maybeV = M.basicUnsafeWrite v i (fromMaybe maybeV)
    basicClear (MV_Maybe v) = M.basicClear v
    basicSet (MV_Maybe v) maybeV = M.basicSet v $ fromMaybe maybeV
    basicUnsafeCopy (MV_Maybe v1) (MV_Maybe v2) = M.basicUnsafeCopy v1 v2
    basicUnsafeGrow (MV_Maybe v) n = MV_Maybe `liftM` M.basicUnsafeGrow v n


instance (U.Unbox a, DefaultValue a) => G.Vector U.Vector (Maybe a) where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (MV_Maybe v) = V_Maybe `liftM` G.basicUnsafeFreeze v
    basicUnsafeThaw (V_Maybe v) = MV_Maybe `liftM` G.basicUnsafeThaw v
    basicLength (V_Maybe v) = G.basicLength v
    basicUnsafeSlice i n (V_Maybe v) = V_Maybe $ G.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_Maybe v) i = toMaybe `liftM` G.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_Maybe mv) (V_Maybe v) = G.basicUnsafeCopy mv v
    elemseq _ maybeV y = G.elemseq (undefined :: U.Vector a) bool
                       $ G.elemseq (undefined :: U.Vector a) x y
        where (bool, x) = fromMaybe maybeV

instance (U.Unbox a, DefaultValue a) => U.Unbox (Maybe a)

toMaybe :: (Bool, a) -> Maybe a
toMaybe (True, x) = Just x
toMaybe _ = Nothing

fromMaybe :: DefaultValue a => Maybe a -> (Bool, a)
fromMaybe (Just x) = (True, x)
fromMaybe Nothing = (False, defaultValue)