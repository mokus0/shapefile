{-# LANGUAGE RecordWildCards #-}
module Database.Shapefile.Shapes.Null where

import Database.Shapefile.Shapes.Class
import qualified Database.Shapefile.ShapeTypes as ST

import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754

data NullShape = NullShape deriving (Eq, Show)

instance ShapeType NullShape
    where
        shapeType _           = ST.NullShape
        decodeShape           = return NullShape
        encodeShape NullShape = return ()
