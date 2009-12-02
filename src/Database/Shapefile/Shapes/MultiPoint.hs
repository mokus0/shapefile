{-# LANGUAGE RecordWildCards #-}
module Database.Shapefile.Shapes.MultiPoint where

import Database.Shapefile.Shapes.Point
import Database.Shapefile.Shapes.Class
import qualified Database.Shapefile.ShapeTypes as ST

import Database.Shapefile.Misc
import Control.Monad
import Data.List

import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754

data MultiPoint = MultiPoint
    { multiPointBox     :: BBox (Double, Double)
    , multiPointPoints  :: [(Double, Double)]
    } deriving (Eq, Show)

instance ShapeType MultiPoint
    where
        shapeType _ = ST.MultiPoint
        decodeShape = do
            bbox <- getBBox (getPair getFloat64le)
            nPts <- getWord32le
            pts  <- replicateM (fromIntegral nPts) (getPair getFloat64le)
            return MultiPoint
                { multiPointBox = bbox
                , multiPointPoints = pts
                }
        encodeShape MultiPoint
                { multiPointBox = bbox
                , multiPointPoints = pts
                } = do
            putBBox (putPair putFloat64le) bbox
            putWord32le (genericLength pts)
            mapM_ (putPair putFloat64le) pts

data MultiPointM = MultiPointM
    { multiPointMBox     :: BBox PointM
    , multiPointMPoints  :: [PointM]
    } deriving (Eq, Show)

instance ShapeType MultiPointM
    where
        shapeType _ = ST.MultiPointM
        decodeShape = do
            MultiPoint bbox pts <- decodeShape
            mbox    <- getBBox getFloat64le
            marray  <- replicateM (length pts) getFloat64le
            return MultiPointM
                { multiPointMBox = zipBBoxWith mkPointM bbox mbox
                , multiPointMPoints = zipWith mkPointM pts marray
                }
            where mkPointM (x,y) m = PointM (x,y,m)
        encodeShape MultiPointM
                { multiPointMBox = bboxM
                , multiPointMPoints = ptsM
                } = do
            let unPointM (PointM (x,y,m)) = ((x,y), m)
                (bbox, mbox) = unzipBBoxWith (unPointM) bboxM
                (pts,marray) = unzip (map unPointM ptsM)
            encodeShape (MultiPoint bbox pts)
            putBBox putFloat64le mbox
            mapM_ putFloat64le marray
