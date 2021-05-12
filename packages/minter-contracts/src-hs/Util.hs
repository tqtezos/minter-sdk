{-# OPTIONS_GHC -Wno-orphans #-}

module Util () where

import Fmt (Buildable(..))
import Lorentz (toVal)

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

instance Buildable () where
  build () = "()"

instance Buildable ByteString where
  build = build . toVal
