{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeFamilies #-}

module Database.Shapefile.Points.ByParts where

import Control.Monad                            (liftM)
import Data.Binary.IEEE754                      (putFloat64le)
import Data.Binary.Put                          (Put, putWord32le)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed                      (Vector, unsafeHead)
import qualified Data.Vector.Unboxed as U

import Database.Shapefile.Misc                  (putPair)
import qualified Database.Shapefile.ShapeTypes as ShpT


data XYPoint = XYPoint  { xyXy :: (Double, Double) }
        deriving (Eq, Show)

data MPoint = MPoint    { mXy :: (Double, Double)
                        , mM :: Double }
        deriving (Eq, Show)

data ZPoint = ZPoint    { zXy :: (Double, Double)
                        , zZ :: Double
                        , zM :: Double }
        deriving (Eq, Show)


class U.Unbox p =>  Point p where
        xy :: p -> (Double, Double)
        hasZ :: p -> Bool
        hasM :: p -> Bool
        pointType :: p -> ShpT.ESRIShapeType
        multiPointType :: p -> ShpT.ESRIShapeType
        polyLineType :: p -> ShpT.ESRIShapeType
        polygonType :: p -> ShpT.ESRIShapeType
        vecMultiPointType :: Vector p -> ShpT.ESRIShapeType
        vecMultiPointType v = multiPointType $ unsafeHead v
        vecPolyLineType :: Vector p -> ShpT.ESRIShapeType
        vecPolyLineType v = polyLineType $ unsafeHead v
        vecPolygonType :: Vector p -> ShpT.ESRIShapeType
        vecPolygonType v = polygonType $ unsafeHead v
        vecVecPolyLineType :: V.Vector (Vector p) -> ShpT.ESRIShapeType
        vecVecPolyLineType vv = vecPolyLineType $ V.unsafeHead vv
        vecVecPolygonType :: V.Vector (Vector p) -> ShpT.ESRIShapeType
        vecVecPolygonType vv = vecPolygonType $ V.unsafeHead vv
        sizeWords :: p -> Int
        putValue :: p -> Put
        putBBoxXY :: BBox p -> Put
        putBBoxXY (BBox {..}) = do putPair putFloat64le (xy min)
                                   putPair putFloat64le (xy max)
        putBBoxZ :: BBox p -> Put
        putBBoxM :: BBox p -> Put
        putPointsXY :: Vector p -> Put
        putPointsXY pts = U.forM_ pts ((putPair putFloat64le) . xy)
        putPointsZ :: Vector p -> Put
        putPointsM :: Vector p -> Put

instance Point XYPoint where
        xy = xyXy
        hasZ _ = False
        hasM _ = False
        pointType _ = ShpT.Point
        multiPointType _ = ShpT.MultiPoint
        polyLineType _ = ShpT.PolyLine
        polygonType _ = ShpT.Polygon
        sizeWords _ = 8
        putValue (XYPoint xy) = putPair putFloat64le xy
        putBBoxZ _ = return ()
        putBBoxM _ = return ()
        putPointsZ _ = return ()
        putPointsM _ = return ()

instance Point MPoint where
        xy = mXy
        hasZ _ = False
        hasM _ = True
        pointType _ = ShpT.PointM
        multiPointType _ = ShpT.MultiPointM
        polyLineType _ = ShpT.PolyLineM
        polygonType _ = ShpT.PolygonM
        sizeWords _ = 12
        putValue (MPoint xy m) = do putPair putFloat64le xy
                                    putFloat64le m
        putBBoxZ _ = return ()
        putBBoxM (BBox (MPoint _ mMin) (MPoint _ mMax)) =
                putPair putFloat64le (mMin, mMax)
        putPointsZ _ = return ()
        putPointsM v = U.forM_ v (putFloat64le . mM)

instance Point ZPoint where
        xy = zXy
        hasZ _ = True
        hasM _ = True
        pointType _ = ShpT.PointZ
        multiPointType _ = ShpT.MultiPointZ
        polyLineType _ = ShpT.PolyLineZ
        polygonType _ = ShpT.PolygonZ
        sizeWords _ = 16
        putValue (ZPoint xy z m) = do putPair putFloat64le xy
                                      putFloat64le z
                                      putFloat64le m
        putBBoxZ (BBox (ZPoint _ zMin _) (ZPoint _ zMax _)) =
                putPair putFloat64le (zMin, zMax)
        putBBoxM (BBox (ZPoint _ mMin _) (ZPoint _ mMax _)) =
                putPair putFloat64le (mMin, mMax)
        putPointsZ v = U.forM_ v (putFloat64le . zZ)
        putPointsM v = U.forM_ v (putFloat64le . zM)

data Point p => BBox p = BBox   { min :: p
                                , max :: p }
                        deriving (Eq, Show)



-- ----------------------------------------------------------------------------
-- Instances of Unboxed for use with Data.Vector
-- (See <http://hackage.haskell.org/packages/archive/vector/0.9.1/doc/html/src/Data-Vector-Unboxed-Base.html>)
-- ----------------------------------------------------------------------------

newtype instance U.MVector s XYPoint = MV_Point (U.MVector s (Double, Double))
newtype instance U.Vector XYPoint    = V_Point  (U.Vector    (Double, Double))

instance U.Unbox XYPoint

instance M.MVector U.MVector XYPoint where
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
        basicLength (MV_Point v) = M.basicLength v
        basicUnsafeSlice i n (MV_Point v) = MV_Point $ M.basicUnsafeSlice i n v
        basicOverlaps (MV_Point v1) (MV_Point v2) = M.basicOverlaps v1 v2
        basicUnsafeNew n = MV_Point `liftM` M.basicUnsafeNew n
        basicUnsafeReplicate n (XYPoint lmV) =
                MV_Point `liftM` M.basicUnsafeReplicate n lmV
        basicUnsafeRead (MV_Point v) i = XYPoint `liftM` M.basicUnsafeRead v i
        basicUnsafeWrite (MV_Point v) i (XYPoint lmV) =
                M.basicUnsafeWrite v i lmV
        basicClear (MV_Point v) = M.basicClear v
        basicSet (MV_Point v) (XYPoint lmV) = M.basicSet v lmV
        basicUnsafeCopy (MV_Point v1) (MV_Point v2) = M.basicUnsafeCopy v1 v2
        basicUnsafeMove (MV_Point v1) (MV_Point v2) = M.basicUnsafeMove v1 v2
        basicUnsafeGrow (MV_Point v) n = MV_Point `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector XYPoint where
        {-# INLINE basicUnsafeFreeze #-}
        {-# INLINE basicUnsafeThaw #-}
        {-# INLINE basicLength #-}
        {-# INLINE basicUnsafeSlice #-}
        {-# INLINE basicUnsafeIndexM #-}
        {-# INLINE elemseq #-}
        basicUnsafeFreeze (MV_Point v) = V_Point `liftM` G.basicUnsafeFreeze v
        basicUnsafeThaw (V_Point v) = MV_Point `liftM` G.basicUnsafeThaw v
        basicLength (V_Point v) = G.basicLength v
        basicUnsafeSlice i n (V_Point v) = V_Point $ G.basicUnsafeSlice i n v
        basicUnsafeIndexM (V_Point v) i =
                XYPoint `liftM` G.basicUnsafeIndexM v i
        basicUnsafeCopy (MV_Point mv) (V_Point v) = G.basicUnsafeCopy mv v
        elemseq _ (XYPoint (x,y)) z = G.elemseq (undefined :: Vector Double) x
                                    $ G.elemseq (undefined :: Vector Double) y z


newtype instance U.MVector s MPoint =
        MV_MPoint (U.MVector s (Double, Double, Double))
newtype instance U.Vector MPoint    =
        V_MPoint  (U.Vector    (Double, Double, Double))

instance U.Unbox MPoint

instance M.MVector U.MVector MPoint where
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
        basicLength (MV_MPoint v) = M.basicLength v
        basicUnsafeSlice i n (MV_MPoint v) =
                MV_MPoint $ M.basicUnsafeSlice i n v
        basicOverlaps (MV_MPoint v1) (MV_MPoint v2) = M.basicOverlaps v1 v2
        basicUnsafeNew n = MV_MPoint `liftM` M.basicUnsafeNew n
        basicUnsafeReplicate n (MPoint {mXy=(x,y), mM=m}) =
                MV_MPoint `liftM` M.basicUnsafeReplicate n (x,y,m)
        basicUnsafeRead (MV_MPoint v) i =
                (\(x,y,m) -> MPoint (x,y) m) `liftM` M.basicUnsafeRead v i
        basicUnsafeWrite (MV_MPoint v) i (MPoint {mXy=(x,y), mM=m}) =
                M.basicUnsafeWrite v i (x,y,m)
        basicClear (MV_MPoint v) = M.basicClear v
        basicSet (MV_MPoint v) (MPoint {mXy=(x,y), mM=m}) = M.basicSet v (x,y,m)
        basicUnsafeCopy (MV_MPoint v1) (MV_MPoint v2) = M.basicUnsafeCopy v1 v2
        basicUnsafeMove (MV_MPoint v1) (MV_MPoint v2) = M.basicUnsafeMove v1 v2
        basicUnsafeGrow (MV_MPoint v) n =
                MV_MPoint `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector MPoint where
        {-# INLINE basicUnsafeFreeze #-}
        {-# INLINE basicUnsafeThaw #-}
        {-# INLINE basicLength #-}
        {-# INLINE basicUnsafeSlice #-}
        {-# INLINE basicUnsafeIndexM #-}
        {-# INLINE elemseq #-}
        basicUnsafeFreeze (MV_MPoint v) = V_MPoint `liftM` G.basicUnsafeFreeze v
        basicUnsafeThaw (V_MPoint v) = MV_MPoint `liftM` G.basicUnsafeThaw v
        basicLength (V_MPoint v) = G.basicLength v
        basicUnsafeSlice i n (V_MPoint v) = V_MPoint $ G.basicUnsafeSlice i n v
        basicUnsafeIndexM (V_MPoint v) i =
                (\(x,y,m) -> MPoint (x,y) m) `liftM` G.basicUnsafeIndexM v i
        basicUnsafeCopy (MV_MPoint mv) (V_MPoint v) = G.basicUnsafeCopy mv v
        elemseq _ (MPoint {mXy=(x,y), mM=m}) z =
                G.elemseq (undefined :: Vector Double) x $
                G.elemseq (undefined :: Vector Double) y $
                G.elemseq (undefined :: Vector Double) m $ z


newtype instance U.MVector s ZPoint =
        MV_ZPoint (U.MVector s (Double, Double, Double, Double))
newtype instance U.Vector ZPoint    =
        V_ZPoint  (U.Vector    (Double, Double, Double, Double))

instance U.Unbox ZPoint

instance M.MVector U.MVector ZPoint where
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
        basicLength (MV_ZPoint v) = M.basicLength v
        basicUnsafeSlice i n (MV_ZPoint v) =
                MV_ZPoint $ M.basicUnsafeSlice i n v
        basicOverlaps (MV_ZPoint v1) (MV_ZPoint v2) = M.basicOverlaps v1 v2
        basicUnsafeNew n = MV_ZPoint `liftM` M.basicUnsafeNew n
        basicUnsafeReplicate n (ZPoint {zXy=(x,y), zZ=z, zM=m}) =
                MV_ZPoint `liftM` M.basicUnsafeReplicate n (x,y,z,m)
        basicUnsafeRead (MV_ZPoint v) i =
                (\(x,y,z,m) -> ZPoint (x,y) z m) `liftM` M.basicUnsafeRead v i
        basicUnsafeWrite (MV_ZPoint v) i (ZPoint {zXy=(x,y), zZ=z, zM=m}) =
                M.basicUnsafeWrite v i (x,y,z,m)
        basicClear (MV_ZPoint v) = M.basicClear v
        basicSet (MV_ZPoint v) (ZPoint {zXy=(x,y), zZ=z, zM=m}) =
                M.basicSet v (x,y,z,m)
        basicUnsafeCopy (MV_ZPoint v1) (MV_ZPoint v2) = M.basicUnsafeCopy v1 v2
        basicUnsafeMove (MV_ZPoint v1) (MV_ZPoint v2) = M.basicUnsafeMove v1 v2
        basicUnsafeGrow (MV_ZPoint v) n =
                MV_ZPoint `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector ZPoint where
        {-# INLINE basicUnsafeFreeze #-}
        {-# INLINE basicUnsafeThaw #-}
        {-# INLINE basicLength #-}
        {-# INLINE basicUnsafeSlice #-}
        {-# INLINE basicUnsafeIndexM #-}
        {-# INLINE elemseq #-}
        basicUnsafeFreeze (MV_ZPoint v) = V_ZPoint `liftM` G.basicUnsafeFreeze v
        basicUnsafeThaw (V_ZPoint v) = MV_ZPoint `liftM` G.basicUnsafeThaw v
        basicLength (V_ZPoint v) = G.basicLength v
        basicUnsafeSlice i n (V_ZPoint v) = V_ZPoint $ G.basicUnsafeSlice i n v
        basicUnsafeIndexM (V_ZPoint v) i =
                (\(x,y,z,m) -> ZPoint (x,y) z m) `liftM` G.basicUnsafeIndexM v i
        basicUnsafeCopy (MV_ZPoint mv) (V_ZPoint v) = G.basicUnsafeCopy mv v
        elemseq _ (ZPoint {zXy=(x,y), zZ=z, zM=m}) zz =
                G.elemseq (undefined :: Vector Double) x $
                G.elemseq (undefined :: Vector Double) y $
                G.elemseq (undefined :: Vector Double) z $
                G.elemseq (undefined :: Vector Double) m $ zz


{-
newtype instance U.MVector s (Vector p) =
        MV_VectorP (U.MVector s (U.Vector p))
newtype instance U.Vector (Vector p)    =
        V_VectorP  (U.Vector    (U.Vector p))

instance Point p => U.Unbox (Vector p)

instance Point p => M.MVector U.MVector (Vector p) where
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
        basicLength (MV_VectorP v) = M.basicLength v
        basicUnsafeSlice i n (MV_VectorP v) =
                MV_VectorP $ M.basicUnsafeSlice i n v
        basicOverlaps (MV_VectorP v1) (MV_VectorP v2) = M.basicOverlaps v1 v2
        basicUnsafeNew n = MV_VectorP `liftM` M.basicUnsafeNew n
        basicUnsafeReplicate n lmV = MV_VectorP `liftM`
                M.basicUnsafeReplicate n lmV
        basicUnsafeRead (MV_VectorP v) i = M.basicUnsafeRead v i
        basicUnsafeWrite (MV_VectorP v) i lmV = M.basicUnsafeWrite v i lmV
        basicClear (MV_VectorP v) = M.basicClear v
        basicSet (MV_VectorP v) lmV = M.basicSet v  lmV
        basicUnsafeCopy (MV_VectorP v1) (MV_VectorP v2) =
                M.basicUnsafeCopy v1 v2
        basicUnsafeMove (MV_VectorP v1) (MV_VectorP v2) =
                M.basicUnsafeMove v1 v2
        basicUnsafeGrow (MV_VectorP v) n =
                MV_VectorP `liftM` M.basicUnsafeGrow v n

instance Point p => G.Vector U.Vector (Vector p) where
        {-# INLINE basicUnsafeFreeze #-}
        {-# INLINE basicUnsafeThaw #-}
        {-# INLINE basicLength #-}
        {-# INLINE basicUnsafeSlice #-}
        {-# INLINE basicUnsafeIndexM #-}
        {-# INLINE elemseq #-}
        basicUnsafeFreeze (MV_VectorP v) =
                V_VectorP `liftM` G.basicUnsafeFreeze v
        basicUnsafeThaw (V_VectorP v) = MV_VectorP `liftM` G.basicUnsafeThaw v
        basicLength (V_VectorP v) = G.basicLength v
        basicUnsafeSlice i n (V_VectorP v) =
                V_VectorP $ G.basicUnsafeSlice i n v
        basicUnsafeIndexM (V_VectorP v) i = G.basicUnsafeIndexM v i
        basicUnsafeCopy (MV_VectorP mv) (V_VectorP v) = G.basicUnsafeCopy mv v
--      elemseq _ lmV z =
--              G.elemseq (undefined :: U.Vector Word32)
--                      (fromIntegral $ fromEnum $ multiPatchPartType lmV) $
--              G.elemseq (undefined :: U.Vector (Vector p)) (pPoints lmV) z
-}
