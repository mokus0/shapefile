{-# LANGUAGE RecordWildCards #-}
module Database.Shapefile 
    ( module Database.Shapefile
    , module Database.Shapefile.Shapes
    , module Database.Shapefile.Shp
    , module Database.Shapefile.Shx
    , module Database.Shapefile.Shp.Handle
    , module Database.Shapefile.Shx.Handle
    , module Database.XBase.Dbf
    , BBox(..)
    ) where

import Database.Shapefile.Misc
import Database.Shapefile.Shapes
import Database.Shapefile.Shp
import Database.Shapefile.Shx
import Database.Shapefile.Shp.Handle
import Database.Shapefile.Shx.Handle
import Database.XBase.Dbf

import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString.Lazy as BS

readShpFile path = do
    file <- BS.readFile path
    return (runGet getShpFile file)

writeShpFile path shp = do
    BS.writeFile path (runPut (uncurry putShpFile shp))

readShxFile path = do
    file <- BS.readFile path
    return (runGet getShxFile file)

writeShxFile path shx = do
    BS.writeFile path (runPut (uncurry putShxFile shx))
