{-# LANGUAGE RecordWildCards #-}
module Database.Shapefile.Shx where

import Database.Shapefile.Shp

import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Data.List
import Control.Monad

-- |offset and length of corresponding shape in 16-bit words
data ShxRec = ShxRec
    { shxOffset     :: Word32
    , shxLength     :: Word32
    } deriving (Eq, Show)

shxRecBytes offBytes lenBytes
    | odd offBytes      = error "shxRecBytes: odd byte offset"
    | offBytes > big    = error "shxRecBytes: offset too large for Word32"
    | odd lenBytes      = error "shxRecBytes: odd byte length"
    | lenBytes > big    = error "shxRecBytes: length too large for Word32"
    | otherwise = ShxRec (b2w offBytes) (b2w lenBytes)
    where
        big = 2 * toInteger (maxBound :: Word32)
        b2w x = fromInteger (x `div` 2)

shxOffsetBytes = (*2) . toInteger . shxOffset
shxLengthBytes = (*2) . toInteger . shxLength

putShxRec ShxRec {..} = do
    {- 0: Offset -}     putWord32be shxOffset
    {- 4: Length -}     putWord32be shxLength
    {- 8 bytes total -}

getShxRec = do
    {- 0: Offset -}     shxOffset <- getWord32be
    {- 4: Length -}     shxLength <- getWord32be
    {- 8 bytes total -} return ShxRec
                            { shxOffset = shxOffset
                            , shxLength = shxLength
                            }

shxFromShp shpHdr shpRecs = (shxHdr, shxRecs)
    where
        nRecs = genericLength shpRecs
        shxHdr = shpHdr { shpFileLength = 50 + nRecs * 4}
        (shpFileLen, shxRecs) = mapAccumL mkShxRec 50 shpRecs
        shpLen = shpRecSize . shpRecHdr
        
        mkShxRec off shp = let len = shpLen shp in (off + 4 + len, ShxRec off len)

putShxFile shxHdr shxRecs = do
    putShpFileHeader shxHdr
    mapM_ putShxRec shxRecs

getShxFile = do
    shxHdr  <- getShpFileHeader
    let nWords = shpFileLength shxHdr
        divExact p q = case p `divMod` q of
            (d,0) -> return d
            _     -> fail ("getShxFile: size in header does not make sense (" ++ show p ++ " words)")
    
    nRecs <- (nWords - 50) `divExact` 4
    shxRecs <- replicateM (fromIntegral nRecs) getShxRec
    return (shxHdr, shxRecs)