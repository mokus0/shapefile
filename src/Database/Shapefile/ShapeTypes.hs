module Database.Shapefile.ShapeTypes where

import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Function (on)

data ESRIShapeType
    = NullShape
    | Point
    | PolyLine
    | Polygon
    | MultiPoint
    | PointZ
    | PolyLineZ
    | PolygonZ
    | MultiPointZ
    | PointM
    | PolyLineM
    | PolygonM
    | MultiPointM
    | MultiPatch
    | Unknown Word32
    deriving (Show, Read)

hasZ PointZ         = True
hasZ PolyLineZ      = True
hasZ PolygonZ       = True
hasZ MultiPointZ    = True
hasZ _              = False

hasM PointM         = True
hasM PolyLineM      = True
hasM PolygonM       = True
hasM MultiPointM    = True
hasM other          = hasZ other

instance Enum ESRIShapeType where
    toEnum 0    = NullShape
    toEnum 1    = Point
    toEnum 3    = PolyLine
    toEnum 5    = Polygon
    toEnum 8    = MultiPoint
    toEnum 11   = PointZ
    toEnum 13   = PolyLineZ
    toEnum 15   = PolygonZ
    toEnum 18   = MultiPointZ
    toEnum 21   = PointM
    toEnum 23   = PolyLineM
    toEnum 25   = PolygonM
    toEnum 28   = MultiPointM
    toEnum 31   = MultiPatch
    toEnum x    = Unknown (toEnum x)
    
    fromEnum NullShape      = 0 
    fromEnum Point          = 1 
    fromEnum PolyLine       = 3 
    fromEnum Polygon        = 5 
    fromEnum MultiPoint     = 8 
    fromEnum PointZ         = 11
    fromEnum PolyLineZ      = 13
    fromEnum PolygonZ       = 15
    fromEnum MultiPointZ    = 18
    fromEnum PointM         = 21
    fromEnum PolyLineM      = 23
    fromEnum PolygonM       = 25
    fromEnum MultiPointM    = 28
    fromEnum MultiPatch     = 31
    fromEnum (Unknown x)    = fromEnum x 

instance Bounded ESRIShapeType where
    minBound = NullShape
    maxBound = Unknown maxBound

instance Eq ESRIShapeType where
    (==) = (==) `on` fromEnum

instance Ord ESRIShapeType where
    compare = compare `on` fromEnum

identifyShapeType :: ESRIShapeType -> ESRIShapeType
identifyShapeType = toEnum . fromEnum

isKnownShapeType t = case identifyShapeType t of
    Unknown _   -> False
    _           -> True

putShapeType32le :: ESRIShapeType -> Put
putShapeType32le = putWord32le . fromIntegral . fromEnum
getShapeType32le :: Get ESRIShapeType
getShapeType32le = fmap (toEnum . fromIntegral) getWord32le
