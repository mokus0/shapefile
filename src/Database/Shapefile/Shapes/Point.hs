{-# LANGUAGE RecordWildCards, FlexibleInstances #-}
module Database.Shapefile.Shapes.Point where

import Database.Shapefile.Shapes.Class
import qualified Database.Shapefile.ShapeTypes as ST

import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754

instance ShapeType (Double, Double)
    where
        shapeType _ = ST.Point
        decodeShape = do
            x <- getFloat64le
            y <- getFloat64le
            return (x,y)
        encodeShape (x,y) = do
            putFloat64le x
            putFloat64le y

newtype PointM = PointM (Double, Double, Double)
    deriving (Eq, Show)

instance ShapeType PointM 
    where
        shapeType _ = ST.PointM
        decodeShape = do
            x <- getFloat64le
            y <- getFloat64le
            m <- getFloat64le
            return (PointM (x,y,m))
        encodeShape (PointM (x,y,m)) = do
            putFloat64le x
            putFloat64le y
            putFloat64le m
