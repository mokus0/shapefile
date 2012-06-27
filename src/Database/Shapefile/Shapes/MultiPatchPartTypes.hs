{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Database.Shapefile.Shapes.MultiPatchPartTypes where

import Control.Monad			(liftM)
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import Data.Word
import Data.Function			(on)

data MultiPatchPartType
    = TriangleStrip
    | TriangleFan
    | OuterRing
    | InnerRing
    | FirstRing
    | Ring
    | Unknown Word32
    deriving (Show, Read)

hasArea :: MultiPatchPartType -> Bool
hasArea TriangleStrip	= True
hasArea TriangleFan	= True
hasArea _		= False

instance Enum MultiPatchPartType where
    toEnum 0    = TriangleStrip
    toEnum 1    = TriangleFan
    toEnum 2    = OuterRing
    toEnum 3    = InnerRing
    toEnum 4    = FirstRing
    toEnum 5    = Ring
    toEnum x    = Unknown (toEnum x)
    
    fromEnum TriangleStrip  = 0 
    fromEnum TriangleFan    = 1 
    fromEnum OuterRing      = 2 
    fromEnum InnerRing      = 3 
    fromEnum FirstRing      = 4 
    fromEnum Ring           = 5
    fromEnum (Unknown x)    = fromEnum x 

instance Bounded MultiPatchPartType where
    minBound = TriangleStrip
    maxBound = Unknown maxBound

instance Eq MultiPatchPartType where
    (==) = (==) `on` fromEnum

instance Ord MultiPatchPartType where
    compare = compare `on` fromEnum

identifyPartType :: MultiPatchPartType -> MultiPatchPartType
identifyPartType = toEnum . fromEnum

isKnownPartType :: MultiPatchPartType -> Bool
isKnownPartType t = case identifyPartType t of
    Unknown _   -> False
    _           -> True

putPartType32le :: MultiPatchPartType -> Put
putPartType32le = putWord32le . fromIntegral . fromEnum
getPartType32le :: Get MultiPatchPartType
getPartType32le = fmap (toEnum . fromIntegral) getWord32le


-- ---------------------------------------------
-- Instance of Unboxed for use with Data.Vector:
-- 	like in <http://hackage.haskell.org/packages/archive/vector/0.9.1/doc/html/src/Data-Vector-Unboxed-Base.html>
-- --------------------------------------------
newtype instance U.MVector s MultiPatchPartType =
	MV_MultiPatchPartType (P.MVector s Word8)
newtype instance U.Vector    MultiPatchPartType =
	V_MultiPatchPartType  (P.Vector    Word8)

instance U.Unbox MultiPatchPartType

instance M.MVector U.MVector MultiPatchPartType where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_MultiPatchPartType v) = M.basicLength v
    basicUnsafeSlice i n (MV_MultiPatchPartType v) =
    	MV_MultiPatchPartType $ M.basicUnsafeSlice i n v
    basicOverlaps (MV_MultiPatchPartType v1) (MV_MultiPatchPartType v2) =
    	M.basicOverlaps v1 v2
    basicUnsafeNew n = MV_MultiPatchPartType `liftM` M.basicUnsafeNew n
    basicUnsafeReplicate n x = MV_MultiPatchPartType `liftM`
    	M.basicUnsafeReplicate n (fromIntegral $ fromEnum x)
    basicUnsafeRead (MV_MultiPatchPartType v) i =
    	(toEnum . fromIntegral) `liftM` M.basicUnsafeRead v i
    basicUnsafeWrite (MV_MultiPatchPartType v) i x =
    	M.basicUnsafeWrite v i (fromIntegral $ fromEnum x)
    basicClear (MV_MultiPatchPartType v) = M.basicClear v
    basicSet (MV_MultiPatchPartType v) x = M.basicSet v
    	(fromIntegral $ fromEnum x)
    basicUnsafeCopy (MV_MultiPatchPartType v1) (MV_MultiPatchPartType v2) =
    	M.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_MultiPatchPartType v1) (MV_MultiPatchPartType v2) =
    	M.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_MultiPatchPartType v) n =
    	MV_MultiPatchPartType `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector MultiPatchPartType where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (MV_MultiPatchPartType v) =
    	V_MultiPatchPartType `liftM` G.basicUnsafeFreeze v
    basicUnsafeThaw (V_MultiPatchPartType v) =
    	MV_MultiPatchPartType `liftM` G.basicUnsafeThaw v
    basicLength (V_MultiPatchPartType v) = G.basicLength v
    basicUnsafeSlice i n (V_MultiPatchPartType v) =
    	V_MultiPatchPartType $ G.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_MultiPatchPartType v) i =
    	(toEnum . fromIntegral) `liftM` G.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_MultiPatchPartType mv) (V_MultiPatchPartType v) =
    	G.basicUnsafeCopy mv v
    elemseq _ = seq
