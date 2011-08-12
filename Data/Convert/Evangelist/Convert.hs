{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Convert.Evangelist.Convert where

import Data.Convert.Evangelist.Context
import Control.Applicative

class (Foreign int ~ ext, Native ext ~ int) => Convert ext int where
    type Foreign int :: *
    type Native ext :: *
    toForeign :: int -> ExpCxt int ext
    toNative :: ext -> ImpCxt ext int

instance Convert () () where
    type Foreign () = ()
    type Native () = ()
    toForeign = pure
    toNative = pure


