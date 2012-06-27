{-# LANGUAGE RecordWildCards #-}
module Database.Shapefile.Shp.Handle
    ( ShpHandle (..)
    , openShp
    , closeShp
    , shpIsOpen
    , shpHeader
    , shpDbfFields
    , getShpRecCnt
    , readShpBlock
    ) where

import Data.Word                        (Word32)
import Database.Shapefile.Shp
import Database.Shapefile.Shx
import Database.Shapefile.Shx.Handle
import Database.XBase.Dbf.Handle

import System.IO
import System.FilePath
import Control.Monad
import Control.Concurrent.RWLock
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get

import Database.Shapefile.Misc          (divExactIO)

data ShpHandle = ShpHandle
    { shpReadOnly   :: Bool
    , shpLock       :: RWLock
    , shpFile       :: Handle
    , shxHandle     :: ShxHandle
    , dbfHandle     :: DbfHandle
    }

withShpFile_ :: ShpHandle -> IOMode -> (Handle -> IO a) -> IO a
withShpFile_ ShpHandle{..} mode action = withLock shpLock (action shpFile)
    where withLock = case mode of
            ReadMode    -> withReadLock
            _           -> withWriteLock

withShpFile :: ShpHandle -> IOMode -> (Handle -> IO a) -> IO a
withShpFile shp@ShpHandle{..} mode action = case (mode, shpReadOnly) of
    (ReadMode, _)   -> allow
    (_, False)      -> allow
    (_, True)       -> deny
    where
        allow = withShpFile_ shp mode action
        deny  = fail "withShpFile: write attempted on shp which was opened as read-only"    

readShpBlock :: ShpHandle -> Integer -> Int -> IO BS.ByteString
readShpBlock shp pos len = withShpFile shp ReadMode $ \file -> do
    hSeek file AbsoluteSeek pos
    BS.hGet file len

openShp :: FilePath -> Bool -> IO ShpHandle
openShp file shpReadOnly = do
    let mode    | shpReadOnly   = ReadMode
                | otherwise     = ReadWriteMode
    shpFile <- openBinaryFile file mode
    shxHandle <- openShx (file `replaceExtension` "shx") shpReadOnly
    dbfHandle <- openDbf (file `replaceExtension` "dbf") shpReadOnly
    
    shpLock     <- newRWLockIO
    return ShpHandle
        { shpReadOnly   = shpReadOnly
        , shpLock       = shpLock
        , shpFile       = shpFile
        , shxHandle     = shxHandle
        , dbfHandle     = dbfHandle
        }

closeShp :: ShpHandle -> IO ()
closeShp shp = do
    withShpFile_ shp WriteMode hClose
    closeShx (shxHandle shp)
    closeDbf (dbfHandle shp)

shpIsOpen :: ShpHandle -> IO Bool
shpIsOpen ShpHandle{..} = hIsOpen shpFile

shpHeader :: ShpHandle -> IO ShpFileHeader
shpHeader shp = do
    hdr <- readShpBlock shp 0 100
    return (runGet getShpFileHeader hdr)

shpDbfFields :: ShpHandle -> IO [DbfFieldHandle]
shpDbfFields = dbfFields . dbfHandle

-- | The the number of shape records in the file (specified by the
--   'ShpHandle'), according to the associated '.shx' file.
getShpRecCnt :: ShpHandle -> IO Integer
getShpRecCnt shp = do
    shxHdr <- shxHeader (shxHandle shp)
    res <- ((shpFileLength shxHdr) - 50) `divExactIO` 4
    return $ toInteger res
