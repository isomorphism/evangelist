{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Convert.Evangelist.Context where

import Data.Convert.Evangelist.Newtype
import Control.Newtype
import Control.Applicative
import Control.Monad

-- this is just a reimplementation of the identity monad
-- with gratuitous use of Control.Newtype, because ha ha why not
newtype PureCxt a = PureCxt { unwrapPure :: a }
instance Newtype (PureCxt a) a where 
    pack = PureCxt
    unpack = unwrapPure
instance Functor PureCxt where fmap = over PureCxt
instance Applicative PureCxt where 
    pure = pack
    (<*>) = liftN2 ($)
instance Monad PureCxt where
    return = pure
    m >>= k = k $ unpack m

-- should probably rethink to what extent I want to separate these...
type family ExpCxt int :: * -> *
type family ImpCxt ext :: * -> *

type instance ExpCxt () = PureCxt
type instance ImpCxt () = PureCxt


class (Functor f, Cxt t ~ f) => Context (f :: * -> *) t where
    type Collapse t :: *
    type Cxt t :: * -> *
    collapse :: t -> Collapse t

-- strip out pure contexts, only needs to look at one layer
instance Context PureCxt (PureCxt a) where
    type Collapse (PureCxt a) = a
    type Cxt (PureCxt a) = PureCxt
    collapse = unwrapPure

-- merge IO contexts using join
instance Context IO (IO (IO a)) where
    type Collapse (IO (IO a)) = IO a
    type Cxt (IO (IO a)) = IO
    collapse = join

-- remove contexts underneath IO... this might need to be recursive. haven't
-- thought through all the ways contexts can stack up yet.
instance Context IO (IO (PureCxt a)) where
    type Collapse (IO (PureCxt a)) = IO a
    type Cxt (IO (PureCxt a)) = IO
    collapse = fmap unwrapPure

-- defer IO on a function to only the result. definitely recursive here.
instance (Context IO (IO b)) => Context IO (IO (a -> b)) where
    type Collapse (IO (a -> b)) = a -> Collapse (IO b)
    type Cxt (IO (a -> b)) = IO
    collapse x y = collapse $ fmap ($ y) x


