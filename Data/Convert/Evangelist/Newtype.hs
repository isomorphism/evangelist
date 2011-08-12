{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Convert.Evangelist.Newtype where

import Control.Newtype
import Control.Applicative
import Control.Monad


across :: (Newtype n o) => (o -> n) -> (o -> n)
across _ = pack

infixl 3 ./
(./) :: (Newtype n o) => (o -> t) -> (n -> t)
(./) fx = fx . unpack

liftN f x = pack $ f ./ x
liftN2 f x y = pack $ f ./ x ./ y
liftN3 f x y z = pack $ f ./ x ./ y ./ z


