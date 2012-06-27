module Database.Shapefile.Shapes.OneToOne where

import Data.Binary.Get                          (Get, getWord32le)
import Data.Binary.Put                          (Put, putWord32le)
import Data.Binary.IEEE754                      (getFloat64le, putFloat64le)
import qualified Data.ByteString.Lazy as LBS
import Data.Vector.Unboxed                      (Vector, replicateM, forM_)
import Database.Shapefile.Misc                  (BBox, getBBox, getPair,
                putBBox, putPair)
import Database.Shapefile.Shapes.MultiPatchPartTypes
                (MultiPatchPartType, getPartType32le, putPartType32le)
import qualified Database.Shapefile.ShapeTypes as ShpT


data ESRIShape
        = NullShape
        | Point         { xy :: (Double, Double) }
        | MultiPoint    { bbox :: BBox (Double, Double)
                        , numPoints :: Int
                        , points :: Vector (Double, Double) }
        | PolyLine      { bbox :: BBox (Double, Double)
                        , numParts :: Int
                        , numPoints :: Int
                        , partStartIdcs :: Vector Int
                        , points :: Vector (Double, Double) }
        | Polygon       { bbox :: BBox (Double, Double)
                        , numParts :: Int
                        , numPoints :: Int
                        , partStartIdcs :: Vector Int
                        , points :: Vector (Double, Double) }
        | PointM        { xy :: (Double, Double)
                        , m :: Double }
        | MultiPointM   { bbox :: BBox (Double, Double)
                        , numPoints :: Int
                        , points :: Vector (Double, Double)
                        , mBnd :: (Double, Double)
                        , mValues :: Vector Double }
        | PolyLineM     { bbox :: BBox (Double, Double)
                        , numParts :: Int
                        , numPoints :: Int
                        , partStartIdcs :: Vector Int
                        , points :: Vector (Double, Double)
                        , mBnd :: (Double, Double)
                        , mValues :: Vector Double }
        | PolygonM      { bbox :: BBox (Double, Double)
                        , numParts :: Int
                        , numPoints :: Int
                        , partStartIdcs :: Vector Int
                        , points :: Vector (Double, Double)
                        , mBnd :: (Double, Double)
                        , mValues :: Vector Double }
        | PointZ        { xy :: (Double, Double)
                        , z :: Double
                        , m :: Double }
        | MultiPointZ   { bbox :: BBox (Double, Double)
                        , numPoints :: Int
                        , points :: Vector (Double, Double)
                        , zBnd :: (Double, Double)
                        , zValues :: Vector Double
                        , mBnd :: (Double, Double)
                        , mValues :: Vector Double }
        | PolyLineZ     { bbox :: BBox (Double, Double)
                        , numParts :: Int
                        , numPoints :: Int
                        , partStartIdcs :: Vector Int
                        , points :: Vector (Double, Double)
                        , zBnd :: (Double, Double)
                        , zValues :: Vector Double
                        , mBnd :: (Double, Double)
                        , mValues :: Vector Double }
        | PolygonZ      { bbox :: BBox (Double, Double)
                        , numParts :: Int
                        , numPoints :: Int
                        , partStartIdcs :: Vector Int
                        , points :: Vector (Double, Double)
                        , zBnd :: (Double, Double)
                        , zValues :: Vector Double
                        , mBnd :: (Double, Double)
                        , mValues :: Vector Double }
        | MultiPatch    { bbox :: BBox (Double, Double)
                        , numParts :: Int
                        , numPoints :: Int
                        , partStartIdcs :: Vector Int
                        , partTypes :: Vector MultiPatchPartType
                        , points :: Vector (Double, Double)
                        , zBnd :: (Double, Double)
                        , zValues :: Vector Double
                        , mBnd :: (Double, Double)
                        , mValues :: Vector Double }
        deriving (Eq, Show)


--data PointList = PointList { container :: ESRIShape
--                         , startIdx :: Int
--                         , len :: Int }

shapeType :: ESRIShape -> ShpT.ESRIShapeType
shapeType NullShape{}   = ShpT.NullShape
shapeType Point{}       = ShpT.Point
shapeType MultiPoint{}  = ShpT.MultiPoint
shapeType PolyLine{}    = ShpT.PolyLine
shapeType Polygon{}     = ShpT.Polygon
shapeType PointM{}      = ShpT.PointM
shapeType MultiPointM{} = ShpT.MultiPointM
shapeType PolyLineM{}   = ShpT.PolyLineM
shapeType PolygonM{}    = ShpT.PolygonM
shapeType PointZ{}      = ShpT.PointZ
shapeType MultiPointZ{} = ShpT.MultiPointZ
shapeType PolyLineZ{}   = ShpT.PolyLineZ
shapeType PolygonZ{}    = ShpT.PolygonZ
shapeType MultiPatch{}  = ShpT.MultiPatch

contentLengthWords :: ESRIShape -> Int
contentLengthWords NullShape                    = 2
contentLengthWords Point {}                     = 10
contentLengthWords MultiPoint {numPoints=ptCnt} = 20 + 8 * ptCnt
contentLengthWords PolyLine {numParts=partCnt, numPoints=ptCnt} =
        22 + 2 * partCnt + 8 * ptCnt
contentLengthWords Polygon {numParts=partCnt, numPoints=ptCnt} =
        22 + 2 * partCnt + 8 * ptCnt
contentLengthWords PointM {}                    = 14
contentLengthWords MultiPointM {numPoints=ptCnt} = 28 + 12 * ptCnt
contentLengthWords PolyLineM {numParts=partCnt, numPoints=ptCnt} =
        30 + 2 * partCnt + 12 * ptCnt
contentLengthWords PolygonM {numParts=partCnt, numPoints=ptCnt} =
        30 + 2 * partCnt + 12 * ptCnt
contentLengthWords PointZ {}                    = 18
contentLengthWords MultiPointZ {numPoints=ptCnt} = 36 + 16 * ptCnt
contentLengthWords PolyLineZ {numParts=partCnt, numPoints=ptCnt} =
        38 + 2 * partCnt + 16 * ptCnt
contentLengthWords PolygonZ {numParts=partCnt, numPoints=ptCnt} =
        38 + 2 * partCnt + 16 * ptCnt
contentLengthWords MultiPatch {numParts=partCnt, numPoints=ptCnt} =
        38 + 4 * partCnt + 16 * ptCnt


getShape :: Get ESRIShape
getShape = do
        shpType <- ShpT.getShapeType32le
        getShapeWithType shpType

getShapeWithType ShpT.NullShape = return NullShape
getShapeWithType ShpT.Point = do
        pt <- getPair getFloat64le
        return $ Point pt
getShapeWithType ShpT.MultiPoint = do
        bbox <- getBBox (getPair getFloat64le)
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        pts <- replicateM ptCnt (getPair getFloat64le)
        return $ MultiPoint bbox ptCnt pts
getShapeWithType ShpT.PolyLine = do
        bbox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        pts <- replicateM ptCnt (getPair getFloat64le)
        return $ PolyLine bbox partCnt ptCnt partIdcs pts
getShapeWithType ShpT.Polygon = do
        bbox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        pts <- replicateM ptCnt (getPair getFloat64le)
        return $ Polygon bbox partCnt ptCnt partIdcs pts
getShapeWithType ShpT.PointM = do
        pt <- getPair getFloat64le
        m <- getFloat64le
        return $ PointM pt m
getShapeWithType ShpT.MultiPointM = do
        bbox <- getBBox (getPair getFloat64le)
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        pts <- replicateM ptCnt (getPair getFloat64le)
        mBnd <- getPair getFloat64le
        mValues <- replicateM ptCnt getFloat64le
        return $ MultiPointM bbox ptCnt pts mBnd mValues
getShapeWithType ShpT.PolyLineM = do
        bbox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        pts <- replicateM ptCnt (getPair getFloat64le)
        mBnd <- getPair getFloat64le
        mValues <- replicateM ptCnt getFloat64le
        return $ PolyLineM bbox partCnt ptCnt partIdcs pts mBnd mValues
getShapeWithType ShpT.PolygonM = do
        bbox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        pts <- replicateM ptCnt (getPair getFloat64le)
        mBnd <- getPair getFloat64le
        mValues <- replicateM ptCnt getFloat64le
        return $ PolygonM bbox partCnt ptCnt partIdcs pts mBnd mValues
getShapeWithType ShpT.PointZ = do
        pt <- getPair getFloat64le
        z <- getFloat64le
        m <- getFloat64le
        return $ PointZ pt z m
getShapeWithType ShpT.MultiPointZ = do
        bbox <- getBBox (getPair getFloat64le)
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        pts <- replicateM ptCnt (getPair getFloat64le)
        zBnd <- getPair getFloat64le
        zValues <- replicateM ptCnt getFloat64le
        mBnd <- getPair getFloat64le
        mValues <- replicateM ptCnt getFloat64le
        return $ MultiPointZ bbox ptCnt pts zBnd zValues mBnd mValues
getShapeWithType ShpT.PolyLineZ = do
        bbox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        pts <- replicateM ptCnt (getPair getFloat64le)
        zBnd <- getPair getFloat64le
        zValues <- replicateM ptCnt getFloat64le
        mBnd <- getPair getFloat64le
        mValues <- replicateM ptCnt getFloat64le
        return $ PolyLineZ bbox partCnt ptCnt partIdcs pts zBnd zValues mBnd
                mValues
getShapeWithType ShpT.PolygonZ = do
        bbox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        pts <- replicateM ptCnt (getPair getFloat64le)
        zBnd <- getPair getFloat64le
        zValues <- replicateM ptCnt getFloat64le
        mBnd <- getPair getFloat64le
        mValues <- replicateM ptCnt getFloat64le
        return $ PolygonZ bbox partCnt ptCnt partIdcs pts zBnd zValues mBnd
                mValues
getShapeWithType ShpT.MultiPatch = do
        bbox <- getBBox (getPair getFloat64le)
        partCnt <- fmap (fromInteger . toInteger) getWord32le
        ptCnt <- fmap (fromInteger . toInteger) getWord32le
        partIdcs <- replicateM partCnt
                (fmap (fromInteger . toInteger) getWord32le)
        partTs <- replicateM partCnt getPartType32le
        pts <- replicateM ptCnt (getPair getFloat64le)
        zBnd <- getPair getFloat64le
        zValues <- replicateM ptCnt getFloat64le
        mBnd <- getPair getFloat64le
        mValues <- replicateM ptCnt getFloat64le
        return $ MultiPatch bbox partCnt ptCnt partIdcs partTs pts zBnd zValues
                mBnd mValues


putShape :: ESRIShape -> Put
putShape shp = do
        let shpType = shapeType shp
        ShpT.putShapeType32le shpType
        putShapeContent shp

putShapeContent NullShape = return ()
putShapeContent (Point xy) = do
        putPair putFloat64le xy
putShapeContent (MultiPoint bbox ptCnt pts) = do
        putBBox (putPair putFloat64le) bbox
        putWord32le (fromIntegral ptCnt)
        forM_ pts (putPair putFloat64le)
putShapeContent (PolyLine bbox partCnt ptCnt partIdcs pts) = do
        putBBox (putPair putFloat64le) bbox
        putWord32le (fromIntegral partCnt)
        putWord32le (fromIntegral ptCnt)
        forM_ partIdcs (putWord32le . fromIntegral)
        forM_ pts (putPair putFloat64le)
putShapeContent (Polygon bbox partCnt ptCnt partIdcs pts) = do
        putBBox (putPair putFloat64le) bbox
        putWord32le (fromIntegral partCnt)
        putWord32le (fromIntegral ptCnt)
        forM_ partIdcs (putWord32le . fromIntegral)
        forM_ pts (putPair putFloat64le)
putShapeContent (PointM xy m) = do
        putPair putFloat64le xy
        putFloat64le m
putShapeContent (MultiPointM bbox ptCnt pts mBnd mValues) = do
        putBBox (putPair putFloat64le) bbox
        putWord32le (fromIntegral ptCnt)
        forM_ pts (putPair putFloat64le)
        putPair putFloat64le mBnd
        forM_ mValues putFloat64le
putShapeContent (PolyLineM bbox partCnt ptCnt partIdcs pts mBnd mValues) = do
        putBBox (putPair putFloat64le) bbox
        putWord32le (fromIntegral partCnt)
        putWord32le (fromIntegral ptCnt)
        forM_ partIdcs (putWord32le . fromIntegral)
        forM_ pts (putPair putFloat64le)
        putPair putFloat64le mBnd
        forM_ mValues putFloat64le
putShapeContent (PolygonM bbox partCnt ptCnt partIdcs pts mBnd mValues) = do
        putBBox (putPair putFloat64le) bbox
        putWord32le (fromIntegral partCnt)
        putWord32le (fromIntegral ptCnt)
        forM_ partIdcs (putWord32le . fromIntegral)
        forM_ pts (putPair putFloat64le)
        putPair putFloat64le mBnd
        forM_ mValues putFloat64le
putShapeContent (PointZ xy z m) = do
        putPair putFloat64le xy
        putFloat64le z
        putFloat64le m
putShapeContent (MultiPointZ bbox ptCnt pts zBnd zValues mBnd mValues) = do
        putBBox (putPair putFloat64le) bbox
        putWord32le (fromIntegral ptCnt)
        forM_ pts (putPair putFloat64le)
        putPair putFloat64le zBnd
        forM_ zValues putFloat64le
        putPair putFloat64le mBnd
        forM_ mValues putFloat64le
putShapeContent (PolyLineZ bbox partCnt ptCnt partIdcs pts zBnd zValues
  mBnd mValues) = do
        putBBox (putPair putFloat64le) bbox
        putWord32le (fromIntegral partCnt)
        putWord32le (fromIntegral ptCnt)
        forM_ partIdcs (putWord32le . fromIntegral)
        forM_ pts (putPair putFloat64le)
        putPair putFloat64le zBnd
        forM_ zValues putFloat64le
        putPair putFloat64le mBnd
        forM_ mValues putFloat64le
putShapeContent (PolygonZ bbox partCnt ptCnt partIdcs pts zBnd zValues
  mBnd mValues) = do
        putBBox (putPair putFloat64le) bbox
        putWord32le (fromIntegral partCnt)
        putWord32le (fromIntegral ptCnt)
        forM_ partIdcs (putWord32le . fromIntegral)
        forM_ pts (putPair putFloat64le)
        putPair putFloat64le zBnd
        forM_ zValues putFloat64le
        putPair putFloat64le mBnd
        forM_ mValues putFloat64le
putShapeContent (MultiPatch bbox partCnt ptCnt partIdcs partTs pts zBnd zValues
  mBnd mValues) = do
        putBBox (putPair putFloat64le) bbox
        putWord32le (fromIntegral partCnt)
        putWord32le (fromIntegral ptCnt)
        forM_ partIdcs (putWord32le . fromIntegral)
        forM_ partTs putPartType32le
        forM_ pts (putPair putFloat64le)
        putPair putFloat64le zBnd
        forM_ zValues putFloat64le
        putPair putFloat64le mBnd
        forM_ mValues putFloat64le
