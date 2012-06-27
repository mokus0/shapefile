{-# LANGUAGE RecordWildCards #-}

module Database.Shapefile.Shp.Raw where

import Database.Shapefile.ShapeTypes    (ESRIShapeType, getShapeType32le)
import qualified Database.Shapefile.ShapeTypes as ShpT
import Database.Shapefile.Shp           (ShpFileHeader (..), ShpRecHeader (..),
                ShpRecord (..), shpFileLengthBytes, shpRecSizeBytes,
                getShpRecHeader, putShpRecHeader, getShpFileHeader,
                putShpFileHeader)
import Database.Shapefile.Shp.Handle    (ShpHandle (..), readShpBlock)
import Database.Shapefile.Shx           (shxLengthBytes, shxOffsetBytes)
import Database.Shapefile.Shx.Handle    (getShxRecord)

import Data.Binary.Get                  (Get, getLazyByteString,
                getRemainingLazyByteString, runGet)
import Data.Binary.Put                  (Put, putLazyByteString, runPut)
import qualified Data.ByteString.Lazy as BS
import Data.Word                        (Word32)
import Database.XBase.Dbf.Handle        (DbfRecHandle, dbfGetRecord)


data ShpRec = ShpRec
    { shpRecHdr             :: ShpRecHeader
    , shpRecData            :: BS.ByteString
    } deriving (Eq, Show)

instance ShpRecord ShpRec where
    shpRecHeader = shpRecHdr

-- |Total size of the shape record in bytes, including the header
shpRecTotalSizeBytes :: ShpRec -> Integer
shpRecTotalSizeBytes = (8 +) . shpRecSizeBytes . shpRecHdr

-- |A shape record type isn't "part of" the header, but every shape format
--  starts with a word indicating the shape type.  This function extracts it.
shpRecShapeType :: ShpRec -> ESRIShapeType
shpRecShapeType ShpRec{..}
    | BS.length shpRecData < 4  = ShpT.NullShape
    | otherwise = runGet getShapeType32le shpRecData

-- |Pack several raw shape records into 'ShpRec's, setting proper record
-- numbers and sizes.
mkShpRecs :: [BS.ByteString] -> [ShpRec]
mkShpRecs recData = zipWith mkShpRec [1..] recData

-- |Pack the data for a shape into a 'ShpRec' with the specified record
-- number
mkShpRec :: Word32 -> BS.ByteString -> ShpRec
mkShpRec n recData = ShpRec (ShpRecHeader n (bsLength recData)) recData
    where
        bsLength bs = case fromIntegral len `divMod` 2 of
            (words, 0) -> words
            (words, _) -> error ("uneven length shape record (" ++ show len ++ " bytes)")
            where len = BS.length bs

putShpRec :: ShpRec -> Put
putShpRec ShpRec {..} = do
    {- 0 : Record Header -}  putShpRecHeader shpRecHdr
    {- 8 : Record content -} putLazyByteString shpRecData
    {- (8 + BS.length shpRecData) bytes total -}

getShpRec :: Get ShpRec
getShpRec = do
    {- 0 : Record Header -}     shpRecHdr@ShpRecHeader {shpRecSize = len} <- getShpRecHeader
    {- 8 : Record content -}    shpRecData <- getLazyByteString (2 * fromIntegral len)
    {- (8 + len) bytes total -} return ShpRec
                                    { shpRecHdr  = shpRecHdr
                                    , shpRecData = shpRecData
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

readShpFile path = do
    file <- BS.readFile path
    return (runGet getShpFile file)

writeShpFile path shp = do
    BS.writeFile path (runPut (uncurry putShpFile shp))



-- Using handles:

getShpRecord :: ShpHandle -> Int -> IO (ShpRec, Maybe DbfRecHandle)
getShpRecord shp n = do
    shxRec <- getShxRecord (shxHandle shp) n
    rec <- readShpBlock shp (shxOffsetBytes shxRec)
                (8 + fromInteger (shxLengthBytes shxRec))
    dbfRec <- dbfGetRecord (dbfHandle shp) (toInteger n)
    return (runGet getShpRec rec, dbfRec)
