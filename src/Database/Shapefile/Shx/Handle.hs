{-# LANGUAGE RecordWildCards #-}
-- |Primarily for use internally by 'ShpHandle'.
-- Each 'ShpHandle' has an 'ShxHandle' that it uses to lookup arbitrary shape
-- records from the .shp file.
module Database.Shapefile.Shx.Handle
    ( ShxHandle
    , openShx
    , closeShx
    , shxIsOpen
    , shxHeader
    , getShxRecord
    ) where

import Database.Shapefile.Shp
import Database.Shapefile.Shx

import System.IO
import Control.Concurrent.RWLock
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get

data ShxHandle = ShxHandle
    { shxReadOnly   :: Bool
    , shxLock       :: RWLock
    , shxFile       :: Handle
    }

withShxFile_ :: ShxHandle -> IOMode -> (Handle -> IO a) -> IO a
withShxFile_ ShxHandle{..} mode action = withLock shxLock (action shxFile)
    where withLock = case mode of
            ReadMode    -> withReadLock
            _           -> withWriteLock

withShxFile :: ShxHandle -> IOMode -> (Handle -> IO a) -> IO a
withShxFile shx@ShxHandle{..} mode action = case (mode, shxReadOnly) of
    (ReadMode, _)   -> allow
    (_, False)      -> allow
    (_, True)       -> deny
    where
        allow = withShxFile_ shx mode action
        deny  = fail "withShxFile: write attempted on shx which was opened as read-only"    

readShxBlock :: ShxHandle -> Integer -> Int -> IO BS.ByteString
readShxBlock shx pos len = withShxFile shx ReadMode $ \file -> do
    hSeek file AbsoluteSeek pos
    BS.hGet file len

openShx :: FilePath -> Bool -> IO ShxHandle
openShx file shxReadOnly = do
    let mode    | shxReadOnly   = ReadMode
                | otherwise     = ReadWriteMode
    shxFile <- openBinaryFile file mode
    
    shxLock     <- newRWLockIO
    return ShxHandle
        { shxReadOnly   = shxReadOnly
        , shxLock       = shxLock
        , shxFile       = shxFile
        }

closeShx :: ShxHandle -> IO ()
closeShx shx = withShxFile_ shx WriteMode hClose

shxIsOpen :: ShxHandle -> IO Bool
shxIsOpen ShxHandle{..} = hIsOpen shxFile

shxHeader :: ShxHandle -> IO ShpFileHeader
shxHeader shx = do
    hdr <- readShxBlock shx 0 100
    return (runGet getShpFileHeader hdr)

shxRecPos n = 100 + 8 * toInteger n

getShxRecord :: ShxHandle -> Int -> IO ShxRec
getShxRecord shx n = do
    rec <- readShxBlock shx (shxRecPos n) 8
    return (runGet getShxRec rec)