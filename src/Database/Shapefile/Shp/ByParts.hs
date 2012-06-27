{-# LANGUAGE RecordWildCards #-}

module Database.Shapefile.Shp.ByParts where

import Database.Shapefile.ShapeTypes    (ESRIShapeType, getShapeType32le)
import Database.Shapefile.Shapes.ByParts
                (ESRIShape, getShape, putShape, shapeType, contentLengthWords)
import Database.Shapefile.Shp           (ShpFileHeader (..), ShpRecHeader (..),
                ShpRecord (..), shpFileLengthBytes, shpRecSizeBytes,
                getShpRecHeader, putShpRecHeader, getShpFileHeader,
                putShpFileHeader)
import Database.Shapefile.Shp.Handle    (ShpHandle (..), readShpBlock)
import Database.Shapefile.Shx           (shxLengthBytes, shxOffsetBytes)
import Database.Shapefile.Shx.Handle    (getShxRecord)

import Data.Binary.Get                  (Get, getLazyByteString,
                getRemainingLazyByteString, runGet)
import Data.Binary.Put                  (Put, runPut)
import Data.Word                        (Word32)
import Database.XBase.Dbf.Handle        (DbfRecHandle, dbfGetRecord)

data ShpRec = ShpRec
    { shpRecHdr         :: ShpRecHeader
    , shpRecShape       :: ESRIShape
    } deriving (Eq, Show)

instance ShpRecord ShpRec where
    shpRecHeader = shpRecHdr

-- |Total size of the shape record in bytes, including the header
shpRecTotalSizeBytes :: ShpRec -> Integer
shpRecTotalSizeBytes = (8 +) . shpRecSizeBytes . shpRecHdr

shpRecShapeType :: ShpRec -> ESRIShapeType
shpRecShapeType ShpRec{shpRecShape=shape} = shapeType shape

-- |Pack several shapes into 'ShpRec's, setting proper record numbers and
--  sizes.
mkShpRecs :: [ESRIShape] -> [ShpRec]
mkShpRecs shapes = zipWith mkShpRec [1..] shapes

-- |Pack the data for a shape into a 'ShpRec' with the specified record number
mkShpRec :: Word32 -> ESRIShape -> ShpRec
mkShpRec n shape =
        ShpRec (ShpRecHeader n (fromIntegral $ contentLengthWords shape)) shape

putShpRec :: ShpRec -> Put
putShpRec ShpRec {..} = do
    {- 0 : Record Header -}  putShpRecHeader shpRecHdr
    {- 8 : Record content -} putShape shpRecShape

getShpRec :: Get ShpRec
getShpRec = do
    {- 0 : Record Header -}     shpRecHdr <- getShpRecHeader
    {- 8 : Record content -}    shpShape <- getShape
    {- (8 + len) bytes total -} return ShpRec
                                    { shpRecHdr  = shpRecHdr
                                    , shpRecShape = shpShape
                                    }

putShpFile :: ShpFileHeader -> [ShpRec] -> Put
putShpFile shpHdr shpRecs = do
    putShpFileHeader shpHdr
    mapM_ putShpRec shpRecs

getShpFile :: Get (ShpFileHeader, [ShpRec])
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

getShpRecord :: ShpHandle -> Int -> IO (ShpRec, Maybe DbfRecHandle)
getShpRecord shp n = do
    shxRec <- getShxRecord (shxHandle shp) n
    rec <- readShpBlock shp (shxOffsetBytes shxRec) (8 + fromInteger (shxLengthBytes shxRec))
    dbfRec <- dbfGetRecord (dbfHandle shp) (toInteger n)
    return (runGet getShpRec rec, dbfRec)

getShpShape :: ShpHandle -> Int -> IO (ESRIShape, Maybe DbfRecHandle)
getShpShape shp n = do
    shxRec <- getShxRecord (shxHandle shp) n
    blk <- readShpBlock shp (shxOffsetBytes shxRec) (8 + fromInteger (shxLengthBytes shxRec))
    dbfRec <- dbfGetRecord (dbfHandle shp) (toInteger n)
    let shpRec@ShpRec{ shpRecShape=shape } = runGet getShpRec blk
    return (shape, dbfRec)
