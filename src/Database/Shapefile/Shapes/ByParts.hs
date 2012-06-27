{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeFamilies #-}

module Database.Shapefile.Shapes.ByParts where

import Control.Monad                            (liftM, forM_)
import Data.Binary.Get                          (Get, getWord32le, skip)
import Data.Binary.Put                          (Put, putWord32le)
import Data.Binary.IEEE754                      (getFloat64le, putFloat64le)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import Data.Vector.Unboxed                      (Vector, unsafeHead)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word                                (Word32)

import Database.Shapefile.Misc                  (getBBox, getPair, putPair)
import qualified Database.Shapefile.Misc as Misc
import Database.Shapefile.Shapes.MultiPatchParts.ByParts
                (MultiPatchPart (..), partConstr, partType)
import Database.Shapefile.Shapes.MultiPatchPartTypes
                (MultiPatchPartType, getPartType32le, putPartType32le)
import Database.Shapefile.Points.ByParts        (Point (..), XYPoint (..),
                MPoint (..), ZPoint (..), BBox (..))
import qualified Database.Shapefile.ShapeTypes as ShpT



data Point p => ESRIBaseShape p
        = NullShape
        | Point         { value :: p }
        | MultiPoint    { bbox :: BBox p
                        , points :: Vector p }
        | PolyLine      { bbox :: BBox p
                        , parts :: V.Vector (Vector p) }
        | Polygon       { bbox :: BBox p
                        , parts :: V.Vector (Vector p) }
        | MultiPatch    { bbox :: BBox p
                        , pParts :: V.Vector (MultiPatchPart p) }
        deriving (Eq, Show)

bShapeType :: Point p => ESRIBaseShape p -> ShpT.ESRIShapeType
bShapeType NullShape                    = ShpT.NullShape
bShapeType (Point v)                    = pointType v
bShapeType (MultiPoint {points=v})      = vecMultiPointType v
bShapeType (PolyLine {parts=v})         = vecVecPolyLineType v
bShapeType (Polygon {parts=v})          = vecVecPolygonType v
bShapeType MultiPatch {}                = ShpT.MultiPatch

bShapePartCnt :: Point p => ESRIBaseShape p -> Int
bShapePartCnt NullShape                 = 0
bShapePartCnt Point {}                  = 1
bShapePartCnt MultiPoint {}             = 1
bShapePartCnt (PolyLine {parts=p})      = V.length p
bShapePartCnt (Polygon {parts=p})       = V.length p
bShapePartCnt (MultiPatch {pParts=p})   = V.length p

bContentLengthWords :: Point p => ESRIBaseShape p -> Int
bContentLengthWords NullShape                   = 2
bContentLengthWords (Point v)                   = 2 + sizeWords v
bContentLengthWords (MultiPoint {points=p})     =
        4 + (2 + U.length p) * (sizeWords $ unsafeHead p)
bContentLengthWords (PolyLine {parts=pv})       =
        6 + 2 * (V.length pv) +
        (2 + partsPtCnt pv) * (sizeWords $ unsafeHead $ V.unsafeHead pv)
bContentLengthWords (Polygon {parts=pv})                =
        6 + 2 * (V.length pv) +
        (2 + partsPtCnt pv) * (sizeWords $ unsafeHead $ V.unsafeHead pv)
bContentLengthWords (MultiPatch {pParts=pv})    =
        6 + 4 * (V.length pv) +
        (2 + pPartsPtCnt pv) * 16 --(sizeWords $ unsafeHead $ V.unsafeHead pv)


data ESRIShape =  ESRIXyShape (ESRIBaseShape XYPoint)
                | ESRIMShape  (ESRIBaseShape MPoint)
                | ESRIZShape  (ESRIBaseShape ZPoint)
        deriving (Eq, Show)

shapeType :: ESRIShape -> ShpT.ESRIShapeType
shapeType (ESRIXyShape shp) = bShapeType shp
shapeType (ESRIMShape  shp) = bShapeType shp
shapeType (ESRIZShape  shp) = bShapeType shp

contentLengthWords :: ESRIShape -> Int
contentLengthWords (ESRIXyShape shp) = bContentLengthWords shp
contentLengthWords (ESRIMShape  shp) = bContentLengthWords shp
contentLengthWords (ESRIZShape  shp) = bContentLengthWords shp


partsSizeWords :: Point p => V.Vector (Vector p) -> Int
partsSizeWords vv = (sizeWords $ unsafeHead $ V.unsafeHead vv) * (partsPtCnt vv)

partsPtCnt :: Point p => V.Vector (Vector p) -> Int
partsPtCnt vv = V.foldl (\l v -> l + (U.length v)) 0 vv

pPartsPtCnt :: Point p => V.Vector (MultiPatchPart p) -> Int
pPartsPtCnt pv = V.foldl (\l v -> l + (U.length $ pPoints v)) 0 pv



getPointsVec :: Point p => ((Double, Double) -> p) -> Int -> Get (Vector p)
getPointsVec ptConstructor ptCnt =
        U.replicateM ptCnt (fmap ptConstructor (getPair getFloat64le))

partEndIdx :: Vector Int -> Int -> Int -> Int
partEndIdx partStartIdcs partIdx ptCnt
        | partIdx < U.length partStartIdcs = partStartIdcs U.! (partIdx + 1)
        | otherwise                        = ptCnt

getMPt :: Get MPoint
getMPt = do xy <- getPair getFloat64le
            m <- getFloat64le
            return $ MPoint xy m

mPtGetCompon :: Integral i => MPoint -> i -> Double
mPtGetCompon (MPoint (x, _) _) 0 = x
mPtGetCompon (MPoint (_, y) _) 1 = y
mPtGetCompon (MPoint _ m)      3 = m

mPtVecIdxCompon :: U.Vector MPoint -> Int -> Double
mPtVecIdxCompon v i = let (lmI, ofs) = i `divMod` 3 in
                          mPtGetCompon (v U.! lmI) ofs

-- | Read 
-- 
--      * a vector of 'ptCnt' (x, y) pairs (`Point[NumPoints] Points' in the
--   ESRI ShapeFile spec), followed by
-- 
--      * a bounding pair (mMin, mMax) (`Double[2] M Range' in the spec),
--      followed by
-- 
--      * 'ptCnt' m values (`Double[NumPoints] M Array' in the spec),
-- 
--   into a pair (Vector MPoint, BBox MPoint).
-- 
--   The mentioned structure is what appears in `.shp' files, and the result is
--   what we use for modelling shapes 'ByParts' in Haskell.
getXYMBBoxPoints :: Misc.BBox (Double, Double) -> Int ->
                    Get (Vector MPoint, BBox MPoint)
getXYMBBoxPoints (Misc.BBox {Misc.bbMin=xyMin, Misc.bbMax=xyMax}) ptCnt
--        | ptCnt < 1 = do (mMin, mMax) <- getPair getFloat64le
--                         return (U.empty,
--                               BBox (MPoint xyMin mMin) (MPoint xyMax mMax))
-- Trying to conserve temporary use of memory by reading all the float64's, and
-- then rearranging them to form the right (x, y, m) triplets seems to be too
-- much of a computing waste!
--      So: We copy the data around . . .
--        | otherwise = do
        = do    xyPoints <- U.replicateM ptCnt (getPair getFloat64le)
                (mMin, mMax) <- getPair getFloat64le
                pts <- U.forM xyPoints getMMkPoint
                return (pts, BBox (MPoint xyMin mMin) (MPoint xyMax mMax))
  where getMMkPoint xy = do m <- getFloat64le
                            return $ MPoint xy m
{-        | otherwise = do (x0, y0) <- getPair getFloat64le
                           ptData0 <- U.replicateM ptCnt getMPt
                           let mMin = mPtVecIdxCompon ptData0 (2 * ptCnt - 2)
                               mMax = mPtVecIdxCompon ptData0 (2 * ptCnt - 1)
                               ptData = U.create $ do
                                v <- U.unsafeThaw ptData0
                                
                           return (ptData,
                                   BBox (MPoint xyMin mMin) (MPoint xyMax mMax))
-}

-- | Read
-- 
--      * a vector of 'ptCnt' (x, y) pairs (`Point[NumPoints] Points' in the
--   ESRI ShapeFile spec), followed by
-- 
--      * a bounding pair (zMin, zMax) (`Double[2] Z Range' in the spec),
--   followed by
-- 
--      * 'ptCnt' z values (`Double[NumPoints] Z Array'), followed by
-- 
--      * (mMin, mMax) (`Double[2] M Range' in the spec), followed by
-- 
--      * 'ptCnt' m values (`Double[NumPoints] M Array' in the spec),
-- 
--   into a pair (Vector MPoint, BBox MPoint).
-- 
--   The mentioned structure is what appears in `.shp' files, and the result is
--   what we use for modelling shapes 'ByParts' in Haskell.
getXYZMBBoxPoints :: Misc.BBox (Double, Double) -> Int ->
                     Get (Vector ZPoint, BBox ZPoint)
getXYZMBBoxPoints (Misc.BBox {Misc.bbMin=xyMin, Misc.bbMax=xyMax}) ptCnt
        = do    xyPoints <- U.replicateM ptCnt (getPair getFloat64le)
                (zMin, zMax) <- getPair getFloat64le
                zPoints <- U.replicateM ptCnt getFloat64le
                (mMin, mMax) <- getPair getFloat64le
                pts <- U.generateM ptCnt
                        (\i -> getMMkPoint (xyPoints U.! i) (zPoints U.! i))
                return (pts,
                        BBox (ZPoint xyMin zMin mMin) (ZPoint xyMax zMax mMax))
  where getMMkPoint xy z = do m <- getFloat64le
                              return $ ZPoint xy z m


getXYParts ptCnt partIdcs = V.generateM partCnt mkPart
  where partCnt = U.length partIdcs
        mkPart i = getPointsVec XYPoint ((partPtCnt i) - (partIdcs U.! i))
        partPtCnt i | i + 1 < partCnt = partIdcs U.! (i + 1)
                    | otherwise       = ptCnt


getShape :: Get ESRIShape
getShape = do
        shpType <- ShpT.getShapeType32le
        getShapeWithType shpType

getShapeWithType ShpT.NullShape = return $ ESRIXyShape NullShape
getShapeWithType ShpT.Point = do
        pt <- getPair getFloat64le
        return $ ESRIXyShape $ Point $ XYPoint pt
getShapeWithType ShpT.MultiPoint = do
        pBBox <- getBBox (getPair getFloat64le)
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        pts <- U.replicateM ptCnt (fmap XYPoint (getPair getFloat64le))
        let bbox = BBox (XYPoint $ Misc.bbMin pBBox)
                        (XYPoint $ Misc.bbMax pBBox)
        return $ ESRIXyShape $ MultiPoint bbox pts
getShapeWithType ShpT.PolyLine = do
        pBBox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- U.replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        skip $ if U.null partIdcs then 0 else U.head partIdcs
        parts <- getXYParts ptCnt partIdcs
        let bbox = BBox (XYPoint $ Misc.bbMin pBBox)
                        (XYPoint $ Misc.bbMax pBBox)
        return $ ESRIXyShape $ PolyLine bbox parts
getShapeWithType ShpT.Polygon = do
        pBBox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- U.replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        skip $ if U.null partIdcs then 0 else U.head partIdcs
        parts <- getXYParts ptCnt partIdcs
        let bbox = BBox (XYPoint $ Misc.bbMin pBBox)
                        (XYPoint $ Misc.bbMax pBBox)
        return $ ESRIXyShape $ Polygon bbox parts
getShapeWithType ShpT.PointM = do
        pt <- getPair getFloat64le
        m <- getFloat64le
        return $ ESRIMShape $ Point $ MPoint pt m
getShapeWithType ShpT.MultiPointM = do
        xyBBox <- getBBox (getPair getFloat64le)
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        (pts, bbox) <- getXYMBBoxPoints xyBBox ptCnt
        return $ ESRIMShape $ MultiPoint bbox pts
getShapeWithType ShpT.PolyLineM = do
        xyBBox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- U.replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        (pts, bbox) <- getXYMBBoxPoints xyBBox ptCnt
        let parts = V.generate partCnt
                        (\i -> U.slice (partIdcs U.! i)
                                (partEndIdx partIdcs i ptCnt) pts)
        return $ ESRIMShape $ PolyLine bbox parts
getShapeWithType ShpT.PolygonM = do
        xyBBox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- U.replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        (pts, bbox) <- getXYMBBoxPoints xyBBox ptCnt
        let parts = V.generate partCnt
                        (\i -> U.slice (partIdcs U.! i)
                                (partEndIdx partIdcs i ptCnt) pts)
        return $ ESRIMShape $ Polygon bbox parts
getShapeWithType ShpT.PointZ = do
        pt <- getPair getFloat64le
        z <- getFloat64le
        m <- getFloat64le
        return $ ESRIZShape $ Point $ ZPoint pt z m
getShapeWithType ShpT.MultiPointZ = do
        xyBBox <- getBBox (getPair getFloat64le)
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        (pts, bbox) <- getXYZMBBoxPoints xyBBox ptCnt
        return $ ESRIZShape $ MultiPoint bbox pts
getShapeWithType ShpT.PolyLineZ = do
        xyBBox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- U.replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        (pts, bbox) <- getXYZMBBoxPoints xyBBox ptCnt
        let parts = V.generate partCnt
                        (\i -> U.slice (partIdcs U.! i)
                                (partEndIdx partIdcs i ptCnt) pts)
        return $ ESRIZShape $ PolyLine bbox parts
getShapeWithType ShpT.PolygonZ = do
        xyBBox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- U.replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        (pts, bbox) <- getXYZMBBoxPoints xyBBox ptCnt
        let parts = V.generate partCnt
                        (\i -> U.slice (partIdcs U.! i)
                                (partEndIdx partIdcs i ptCnt) pts)
        return $ ESRIZShape $ Polygon bbox parts
getShapeWithType ShpT.MultiPatch = do
        xyBBox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- U.replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        partTs <- U.replicateM partCnt getPartType32le
        (pts, bbox) <- getXYZMBBoxPoints xyBBox ptCnt
        let parts = V.generate partCnt
                (\i -> partConstr
                  (partTs U.! i)
                  (U.slice (partIdcs U.! i) (partEndIdx partIdcs i ptCnt) pts))
        return $ ESRIZShape $ MultiPatch bbox parts


putPartsStartIdcs :: Point p => V.Vector (Vector p) -> Put
putPartsStartIdcs vv = V.foldM_ procsLm 0 vv
  where procsLm l v = do putWord32le $ fromIntegral l
                         return $ l + (U.length v)

putPPartsStartIdcs :: Point p => V.Vector (MultiPatchPart p) -> Put
putPPartsStartIdcs vv = V.foldM_ procsLm 0 vv
  where procsLm l v = do putWord32le $ fromIntegral l
                         return $ l + (U.length $ pPoints v)

putShape :: ESRIShape -> Put
putShape (ESRIXyShape shp) = do
        ShpT.putShapeType32le $ bShapeType shp
        putShapeContent shp
putShape (ESRIMShape shp) = do
        ShpT.putShapeType32le $ bShapeType shp
        putShapeContent shp
putShape (ESRIZShape shp) = do
        ShpT.putShapeType32le $ bShapeType shp
        putShapeContent shp

putShapeContent NullShape = return ()
putShapeContent (Point v) = putValue v
putShapeContent (MultiPoint bbox pts) = do
        putBBoxXY bbox
        putWord32le $ fromIntegral $ U.length pts
        putPointsXY pts
        putBBoxZ bbox
        putPointsZ pts
        putBBoxM bbox
        putPointsM pts
putShapeContent (PolyLine bbox parts) = do
        putBBoxXY bbox
        putWord32le $ fromIntegral $ V.length parts
        putWord32le $ fromIntegral $ partsPtCnt parts
        putPartsStartIdcs parts
        V.forM_ parts putPointsXY
        putBBoxZ bbox
        V.forM_ parts putPointsZ
        putBBoxM bbox
        V.forM_ parts putPointsM
putShapeContent (Polygon bbox parts) = do
        putBBoxXY bbox
        putWord32le $ fromIntegral $ V.length parts
        putWord32le $ fromIntegral $ partsPtCnt parts
        putPartsStartIdcs parts
        V.forM_ parts putPointsXY
        putBBoxZ bbox
        V.forM_ parts putPointsZ
        putBBoxM bbox
        V.forM_ parts putPointsM
putShapeContent (MultiPatch bbox parts) = do
        putBBoxXY bbox
        putWord32le $ fromIntegral $ V.length parts
        putWord32le $ fromIntegral $ pPartsPtCnt parts
        putPPartsStartIdcs parts
        V.forM_ parts (putPartType32le . partType)
        V.forM_ parts (putPointsXY . pPoints)
        putBBoxZ bbox
        V.forM_ parts (putPointsZ . pPoints)
        putBBoxM bbox
        V.forM_ parts (putPointsM . pPoints)
