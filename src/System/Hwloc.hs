{-# LANGUAGE ForeignFunctionInterface #-}
module System.Hwloc
   ( getApiVersion
   )
where

import Data.Word
import Data.Int

-- | Indicate at runtime which hwloc API version was used at build time.
foreign import ccall "hwloc_get_api_version" getApiVersion :: Word
