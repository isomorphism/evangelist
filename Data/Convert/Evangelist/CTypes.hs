{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Convert.Evangelist.CTypes ( module Data.Convert.Evangelist.CTypes 
                                      , module Foreign.C
                                      ) where

import Data.Convert.Evangelist.Context
import Data.Convert.Evangelist.Convert
import Data.Convert.Evangelist
import Control.Newtype
import Control.Applicative
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array


type instance ExpCxt Int = PureCxt
type instance ImpCxt CInt = PureCxt
instance Convert CInt Int where
    type Foreign Int = CInt
    type Native CInt = Int
    toForeign = pure . fromIntegral
    toNative = pure . fromIntegral

type instance ExpCxt Double = PureCxt
type instance ImpCxt CDouble = PureCxt
instance Convert CDouble Double where
    type Foreign Double = CDouble
    type Native CDouble = Double
    toForeign = pure . realToFrac
    toNative = pure . realToFrac

type instance ExpCxt Float = PureCxt
type instance ImpCxt CFloat = PureCxt
instance Convert CFloat Float where
    type Foreign Float = CFloat
    type Native CFloat = Float
    toForeign = pure . realToFrac
    toNative = pure . realToFrac


type instance ExpCxt String = IO
type instance ImpCxt CString = IO
instance Convert CString String where
    type Foreign String = CString
    type Native CString = String
    toForeign = newCString
    toNative = peekCString

-- a quick and dirty way to represent arrays; the Int is because we need a size
-- to convert from an array, and the newtype is because otherwise instances
-- for [a] would overlap with String (which is actually [Char])
-- it would probably be nice to handle the (Int,_) as a context, actually,
-- but I'm not sure what the most effective way to do that would be.
type SizedArray a = (Int, Ptr a)
newtype AsArray a = AsArray { getSizedArray :: [a] }
instance Newtype (AsArray a) [a] where
    pack = AsArray
    unpack = getSizedArray

type instance ExpCxt (AsArray a) = IO
type instance ImpCxt (SizedArray a) = IO
instance (Storable a) => Convert (SizedArray a) (AsArray a) where
    type Foreign (AsArray a) = (SizedArray a)
    type Native (SizedArray a) = (AsArray a) 
    toForeign xs = let xs' = unpack xs in (,) (length xs') <$> newArray xs'
    toNative = fmap pack . uncurry peekArray
