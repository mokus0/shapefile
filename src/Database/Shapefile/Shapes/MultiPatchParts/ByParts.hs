{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Database.Shapefile.Shapes.MultiPatchParts.ByParts where

import Control.Monad                            (liftM)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed                      (Vector)
import qualified Data.Vector.Unboxed as U
import Data.Word                                (Word32)

import Database.Shapefile.Shapes.MultiPatchPartTypes
                (MultiPatchPartType)
import qualified Database.Shapefile.Shapes.MultiPatchPartTypes as MPPT
import Database.Shapefile.Points.ByParts        (Point)

data Point p => MultiPatchPart p
        = TriangleStrip { pPoints :: Vector p }
        | TriangleFan   { pPoints :: Vector p }
        | OuterRing     { pPoints :: Vector p }
        | InnerRing     { pPoints :: Vector p }
        | FirstRing     { pPoints :: Vector p }
        | Ring          { pPoints :: Vector p }
        deriving (Eq, Show)

partType :: Point p => MultiPatchPart p -> MultiPatchPartType
partType (TriangleStrip _)      = MPPT.TriangleStrip
partType (TriangleFan _)        = MPPT.TriangleFan
partType (OuterRing _)          = MPPT.OuterRing
partType (InnerRing _)          = MPPT.InnerRing
partType (FirstRing _)          = MPPT.FirstRing
partType (Ring _)               = MPPT.Ring

partConstr :: Point p => MultiPatchPartType -> (Vector p -> MultiPatchPart p)
partConstr MPPT.TriangleStrip   = TriangleStrip
partConstr MPPT.TriangleFan     = TriangleFan
partConstr MPPT.OuterRing       = OuterRing
partConstr MPPT.InnerRing       = InnerRing
partConstr MPPT.FirstRing       = FirstRing
partConstr MPPT.Ring            = Ring



-- ----------------------------------------------------------------------------
-- Instances of Unboxed for use with Data.Vector
-- (See <http://hackage.haskell.org/packages/archive/vector/0.9.1/doc/html/src/Data-Vector-Unboxed-Base.html>)
-- ----------------------------------------------------------------------------


{-
newtype instance U.MVector s (MultiPatchPart p) =
        MV_MultiPatchPart (U.MVector s (Word32, U.Vector p))
newtype instance U.Vector (MultiPatchPart p)    =
        V_MultiPatchPart  (U.Vector    (Word32, U.Vector p))

instance Point p => U.Unbox (MultiPatchPart p)

instance Point p => M.MVector U.MVector (MultiPatchPart p) where
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
        basicLength (MV_MultiPatchPart v) = M.basicLength v
        basicUnsafeSlice i n (MV_MultiPatchPart v) =
                MV_MultiPatchPart $ M.basicUnsafeSlice i n v
        basicOverlaps (MV_MultiPatchPart v1) (MV_MultiPatchPart v2) =
                M.basicOverlaps v1 v2
        basicUnsafeNew n = MV_MultiPatchPart `liftM` M.basicUnsafeNew n
        basicUnsafeReplicate n lmV = MV_MultiPatchPart `liftM`
                M.basicUnsafeReplicate n
                        (fromIntegral $ fromEnum $ partType lmV, pPoints lmV)
        basicUnsafeRead (MV_MultiPatchPart v) i =
                (\(c,p) -> (partConstr $ toEnum $ fromIntegral c) p) `liftM`
                M.basicUnsafeRead v i
        basicUnsafeWrite (MV_MultiPatchPart v) i lmV =
                M.basicUnsafeWrite v i
                        (fromIntegral $ fromEnum $ partType lmV, pPoints lmV)
        basicClear (MV_MultiPatchPart v) = M.basicClear v
        basicSet (MV_MultiPatchPart v) lmV =
                M.basicSet v (fromIntegral $ fromEnum $ partType lmV,
                              pPoints lmV)
        basicUnsafeCopy (MV_MultiPatchPart v1) (MV_MultiPatchPart v2) =
                M.basicUnsafeCopy v1 v2
        basicUnsafeMove (MV_MultiPatchPart v1) (MV_MultiPatchPart v2) =
                M.basicUnsafeMove v1 v2
        basicUnsafeGrow (MV_MultiPatchPart v) n =
                MV_MultiPatchPart `liftM` M.basicUnsafeGrow v n

instance Point p => G.Vector U.Vector (MultiPatchPart p) where
        {-# INLINE basicUnsafeFreeze #-}
        {-# INLINE basicUnsafeThaw #-}
        {-# INLINE basicLength #-}
        {-# INLINE basicUnsafeSlice #-}
        {-# INLINE basicUnsafeIndexM #-}
        {-# INLINE elemseq #-}
        basicUnsafeFreeze (MV_MultiPatchPart v) =
                V_MultiPatchPart `liftM` G.basicUnsafeFreeze v
        basicUnsafeThaw (V_MultiPatchPart v) =
                MV_MultiPatchPart `liftM` G.basicUnsafeThaw v
        basicLength (V_MultiPatchPart v) = G.basicLength v
        basicUnsafeSlice i n (V_MultiPatchPart v) =
                V_MultiPatchPart $ G.basicUnsafeSlice i n v
        basicUnsafeIndexM (V_MultiPatchPart v) i =
                (\(c,p) -> (partConstr $ toEnum $ fromIntegral c) p) `liftM`
                G.basicUnsafeIndexM v i
        basicUnsafeCopy (MV_MultiPatchPart mv) (V_MultiPatchPart v) =
                G.basicUnsafeCopy mv v
--      elemseq _ lmV z =
--              G.elemseq (undefined :: U.Vector Word32)
--                      (fromIntegral $ fromEnum $ multiPatchPartType lmV) $
--              G.elemseq (undefined :: U.Vector (Vector p)) (pPoints lmV) z
-}
