{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Convert.Evangelist.ImportExport where

import Data.Convert.Evangelist.Context
import Data.Convert.Evangelist.Convert
import Control.Applicative

-- not sure if separating these is really worthwhile or not
class FFImport ext where
    type Import ext :: *
    ffImport :: ext -> Import ext

class FFExport int where
    type Export int :: *
    ffExport :: int -> Export int


instance ( Context IO (IO (ImpCxt a (Native a)))
         , Convert a (Native a)
         ) => FFImport (IO a) where
    type Import (IO a) = Collapse (IO (ImpCxt a (Native a)))
    ffImport x = collapse $ toNative <$> x

instance ( FFImport b, Convert a (Native a)
         , Context (ExpCxt (Native a)) (ExpCxt (Native a) (Import b))
         ) => FFImport (a -> b) where
    type Import (a -> b) = Native a -> Collapse (ExpCxt (Native a) (Import b))
    ffImport f x = collapse $ ffImport . f <$> toForeign x


instance ( Context IO (IO (ExpCxt a (Foreign a)))
         , Convert (Foreign a) a
         ) => FFExport (IO a) where
    type Export (IO a) = Collapse (IO (ExpCxt a (Foreign a)))
    ffExport x = collapse $ toForeign <$> x

instance ( FFExport b, Convert (Foreign a) a
         , Context (ImpCxt (Foreign a)) (ImpCxt (Foreign a) (Export b))
         ) => FFExport (a -> b) where
    type Export (a -> b) = Foreign a -> Collapse (ImpCxt (Foreign a) (Export b))
    ffExport f x = collapse $ ffExport . f <$> toNative x

