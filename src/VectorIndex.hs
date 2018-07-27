module VectorIndex
  where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

vectorIndex :: V.Vector a -> Int -> a
vectorIndex = (V.unsafeIndex)

uVectorIndex :: UV.Unbox a => UV.Vector a -> Int -> a
uVectorIndex = (UV.unsafeIndex)
