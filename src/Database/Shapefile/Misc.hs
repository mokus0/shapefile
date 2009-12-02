{-# LANGUAGE RecordWildCards #-}
module Database.Shapefile.Misc where

import Data.Binary
import Data.Binary.Get

expecting :: (Eq t, Show t) => Get t -> t -> Get ()
get `expecting` result = do
    off <- bytesRead
    x <- get
    if (x == result) then return ()
        else fail $ unwords 
            ["Found", show x, "at offset", show off, "but expected", show result]

putPair :: (a -> Put) -> (a,a) -> Put
putPair putPart (x,y) = do
    putPart x
    putPart y
getPair :: Get a -> Get (a,a)
getPair getPart = do
    x <- getPart
    y <- getPart
    return (x,y)

putBBox :: (a -> Put) -> BBox a -> Put
putBBox putPoint BBox {..} = do
    putPoint bbMin
    putPoint bbMax
getBBox :: Get a -> Get (BBox a)
getBBox getPoint = do
    bbMin <- getPoint
    bbMax <- getPoint
    return (BBox bbMin bbMax)

zipBBoxWith f (BBox min1 max1) (BBox min2 max2) = BBox (f min1 min2) (f max1 max2)
unzipBBoxWith f (BBox min max) = (BBox min1 max1, BBox min2 max2)
    where
        (min1, min2) = f min
        (max1, max2) = f max

data BBox point = BBox
    { bbMin :: point
    , bbMax :: point
    } deriving (Eq, Show)

