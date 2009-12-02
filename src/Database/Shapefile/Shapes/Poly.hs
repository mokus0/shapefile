{-# LANGUAGE RecordWildCards #-}
module Database.Shapefile.Shapes.Poly (PolyLine(..), Polygon(..)) where

import Database.Shapefile.Shapes.Class
import qualified Database.Shapefile.ShapeTypes as ST

import Database.Shapefile.Misc
import Control.Monad
import Data.List

import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754

data PolyLine = PolyLine
    { polyLineBox     :: BBox (Double, Double)
    , polyLinePoints  :: [[(Double, Double)]]
    } deriving (Eq, Show)

instance ShapeType PolyLine
    where
        shapeType _ = ST.PolyLine
        decodeShape = do
            (bbox, pts) <- getPoly
            return PolyLine
                { polyLineBox = bbox
                , polyLinePoints = pts
                }
        encodeShape PolyLine
                { polyLineBox = bbox
                , polyLinePoints = pts
                } = putPoly bbox pts

data Polygon = Polygon
    { polygonBox    :: BBox (Double, Double)
    , polygonPoints :: [[(Double, Double)]]
    } deriving (Eq, Show)

instance ShapeType Polygon
    where
        shapeType _ = ST.Polygon
        decodeShape = do
            (bbox, pts) <- getPoly
            return Polygon
                { polygonBox = bbox
                , polygonPoints = pts
                }
        encodeShape Polygon
                { polygonBox = bbox
                , polygonPoints = pts
                } = putPoly bbox pts

getPoly = do
    bbox    <- getBBox (getPair getFloat64le)
    nParts  <- getWord32le
    nPts    <- getWord32le
    parts   <- replicateM (fromIntegral nParts) getWord32le
    pts     <- sequence
        [ replicateM (fromIntegral n) (getPair getFloat64le)
        | n <- partLengths parts nPts
        ]
    return (bbox, pts)

putPoly bbox pts = do
    putBBox (putPair putFloat64le) bbox
    putWord32le (genericLength pts)
    putWord32le (genericLength (concat pts))
    mapM_ putWord32le (partOffsets pts)
    mapM_ (putPair putFloat64le) (concat pts)

partOffsets pts = take nParts (scanl (+) 0 (map genericLength pts))
    where nParts = length pts

partLengths pts n = zipWith subtract pts (tail pts ++ [n])