{-# LANGUAGE RecordWildCards #-}
module Database.Shapefile.Shp where

import Database.Shapefile.ShapeTypes
import Database.Shapefile.Misc

import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.Word
import Data.Maybe
import qualified Data.ByteString.Lazy as BS

data ShpFileHeader = ShpFileHeader
    { shpFileLength         :: Word32   -- in Word16s.  unsigned, I assume - spec doesn't say
    , shpFileShapeType      :: ESRIShapeType
    , shpFileBBox           :: BBox  (Double, Double)
    , shpFileZBnd           :: Maybe (Double, Double)
    , shpFileMBnd           :: Maybe (Double, Double)
    } deriving (Eq, Show)

shpFileLengthBytes = (2*) . toInteger . shpFileLength

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
    { shpRecNum             :: Word32
    , shpRecSize            :: Word32
    } deriving (Eq, Show)

shpRecSizeBytes = (*2) . toInteger . shpRecSize

putShpRecHeader ShpRecHeader {..} = do
    {- 0 : Record Number -}     putWord32be shpRecNum
    {- 4 : Content Length -}    putWord32be shpRecSize

getShpRecHeader = do
    {- 0 : Record Number -}     shpRecNum  <- getWord32be
    {- 4 : Content Length -}    shpRecSize <- getWord32be
                                return ShpRecHeader
                                    { shpRecNum  = shpRecNum
                                    , shpRecSize = shpRecSize
                                    }

data ShpRec = ShpRec
    { shpRecHdr             :: ShpRecHeader
    , shpRecData            :: BS.ByteString
    } deriving (Eq, Show)

shpRecTotalSizeBytes = (8 +) . shpRecSizeBytes . shpRecHdr

-- NB: it isn't "part of" the header, but every shape format starts with
-- a word indicating the shape type.
shpRecShapeType ShpRec{..}
    | BS.length shpRecData < 4  = NullShape
    | otherwise = runGet getShapeType32le shpRecData
    

mkShpRecs recData = zipWith mkShpRec [0..] recData

mkShpRec n recData = ShpRec (ShpRecHeader n (bsLength recData)) recData
    where
        bsLength bs = case fromIntegral len `divMod` 2 of
            (words, 0) -> words
            (words, _) -> error ("uneven length shape record (" ++ show len ++ " bytes)")
            where len = BS.length bs

putShpRec ShpRec {..} = do
    {- 0 : Record Header -}  putShpRecHeader shpRecHdr
    {- 8 : Record content -} putLazyByteString shpRecData
    {- (8 + BS.length shpRecData) bytes total -}

getShpRec = do
    {- 0 : Record Header -}     shpRecHdr@ShpRecHeader {shpRecSize = len} <- getShpRecHeader
    {- 8 : Record content -}    shpRecData <- getLazyByteString (2 * fromIntegral len)
    {- (8 + len) bytes total -} return ShpRec
                                    { shpRecHdr  = shpRecHdr
                                    , shpRecData = shpRecData
                                    }

putShpFile shpHdr shpRecs = do
    putShpFileHeader shpHdr
    mapM_ putShpRec shpRecs

getShpFile = do
    hdr <- getShpFileHeader
    rest <- getLazyByteString (fromInteger (shpFileLengthBytes hdr) - 100)
    let n = shpFileLengthBytes hdr - 100
    return (hdr, slurp n rest)
    
    where
        slurp 0 rest = []
        slurp n rest = flip runGet rest $ do
            rec <- getShpRec
            rest <- getRemainingLazyByteString
            let n' = n - shpRecTotalSizeBytes rec
            return (rec : slurp n' rest)

