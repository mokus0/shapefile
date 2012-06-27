{-# LANGUAGE RecordWildCards #-}
module Database.Shapefile.Shp where

import Database.Shapefile.ShapeTypes    (ESRIShapeType, getShapeType32le,
                putShapeType32le, hasM, hasZ)
import Database.Shapefile.Misc

import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.Word
import Data.Maybe
import qualified Data.ByteString.Lazy as BS

data ShpFileHeader = ShpFileHeader
    { -- |File length in 16-bit words.  Unsigned, I assume - spec doesn't say.
      shpFileLength         :: Word32
    , shpFileShapeType      :: ESRIShapeType
    , shpFileBBox           :: BBox  (Double, Double)
    , shpFileZBnd           :: Maybe (Double, Double)
    , shpFileMBnd           :: Maybe (Double, Double)
    } deriving (Eq, Show)

-- |Shape file length in bytes
shpFileLengthBytes :: ShpFileHeader -> Integer
shpFileLengthBytes = (2*) . toInteger . shpFileLength

putShpFileHeader :: ShpFileHeader -> Put
putShpFileHeader ShpFileHeader {..} = do
    {-  0: File Code -}     putWord32be 9994
    {-  4: Unused -}        putWord32be 0
    {-  8: Unused -}        putWord32be 0
    {- 12: Unused -}        putWord32be 0
    {- 16: Unused -}        putWord32be 0
    {- 20: Unused -}        putWord32be 0
    {- 24: File Length -}   putWord32be shpFileLength
    {- 28: Version -}       putWord32le 1000
    {- 32: Shape Type -}    putShapeType32le shpFileShapeType
    {- 36: Bounding Box -}  putBBox (putPair putFloat64le) shpFileBBox
    {- 68: Z Bounds -}      putPair putFloat64le (fromMaybe (0,0) shpFileZBnd)
    {- 84: M Bounds -}      putPair putFloat64le (fromMaybe (0,0) shpFileMBnd)
    {- 100 bytes total -}

getShpFileHeader :: Get ShpFileHeader
getShpFileHeader = do
    {-  0: File Code -}     getWord32be `expecting` 9994
    {-  4: Unused -}        getWord32be `expecting` 0
    {-  8: Unused -}        getWord32be `expecting` 0
    {- 12: Unused -}        getWord32be `expecting` 0
    {- 16: Unused -}        getWord32be `expecting` 0
    {- 20: Unused -}        getWord32be `expecting` 0
    {- 24: File Length -}   shpFileLength       <- getWord32be 
    {- 28: Version -}       getWord32le `expecting` 1000
    {- 32: Shape Type -}    shpFileShapeType    <- getShapeType32le
    {- 36: Bounding Box -}  shpFileBBox         <- getBBox (getPair getFloat64le)
    {- 68: Z Bounds -}      shpFileZBnd         <- getPair getFloat64le
    {- 84: M Bounds -}      shpFileMBnd         <- getPair getFloat64le
    {- 100 bytes total -}
                            return $ ShpFileHeader
                                { shpFileLength     = shpFileLength
                                , shpFileShapeType  = shpFileShapeType
                                , shpFileBBox       = shpFileBBox
                                , shpFileZBnd       = if hasZ shpFileShapeType || nonZero shpFileZBnd
                                    then Just shpFileZBnd
                                    else Nothing
                                , shpFileMBnd       = if hasM shpFileShapeType || nonZero shpFileMBnd
                                    then Just shpFileMBnd
                                    else Nothing
                                }
    where 
        nonZero (0,0) = False
        nonZero _ = True


data ShpRecHeader = ShpRecHeader
    { -- |Index of the record.  First index is 1.
      shpRecNum             :: Word32
    , -- |Size of the record in 16-bit words, excluding this header.
      shpRecSize            :: Word32
    } deriving (Eq, Show)

-- |Size of the record in bytes, excluding the record header
shpRecSizeBytes :: ShpRecHeader -> Integer
shpRecSizeBytes = (*2) . toInteger . shpRecSize

putShpRecHeader :: ShpRecHeader -> Put
putShpRecHeader ShpRecHeader {..} = do
    {- 0 : Record Number -}     putWord32be shpRecNum
    {- 4 : Content Length -}    putWord32be shpRecSize

getShpRecHeader :: Get ShpRecHeader
getShpRecHeader = do
    {- 0 : Record Number -}     shpRecNum  <- getWord32be
    {- 4 : Content Length -}    shpRecSize <- getWord32be
                                return ShpRecHeader
                                    { shpRecNum  = shpRecNum
                                    , shpRecSize = shpRecSize
                                    }

class ShpRecord r where
    shpRecHeader :: r -> ShpRecHeader
