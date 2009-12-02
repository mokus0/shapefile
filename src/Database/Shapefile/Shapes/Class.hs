module Database.Shapefile.Shapes.Class where

import Data.ByteString.Lazy
import Data.Binary.Get
import Data.Binary.Put
import Database.Shapefile.ShapeTypes

class ShapeType s where
    shapeType       :: s -> ESRIShapeType
    decodeShape     :: Get s
    encodeShape     :: s -> Put
