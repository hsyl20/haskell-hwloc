{-# LANGUAGE ForeignFunctionInterface #-}
module System.Hwloc.Bitmap
   ( Bitmap
   , allocate
   , allocateFull
   , free
   , dup
   , copy
   , snprintf
   , snprintfList
   , snprintfTaskset
   , asprintf
   , asprintfList
   , asprintfTaskset
   , sscanf
   , sscanfList
   , sscanfTaskset
   , zero
   , fill
   , singleton
   , allBut
   , setup
   , setupWithMask
   , setBit
   , clearBit
   , setRange
   , clearRange
   , setSubset
   , singlify
   , getFirstBits
   , getFirstBit
   , getBitAfter
   , getLastBit
   , getSubset
   , testBit
   , null
   , full
   , union
   , intersection
   , difference
   , symetricDifference
   , negate
   , compare
   , compareFirst
   , intersects
   , isSubsetOf
   , isEqualTo
   , getWeight
   )
where

import Prelude hiding (null, compare, negate)

import Foreign.Ptr
import Foreign.C.Types (CSize(..))
import Foreign.C.String (CString)
import Data.Word

newtype Bitmap = Bitmap (Ptr ())

-- | Allocate a new empty bitmap
-- 
-- Returns A valid bitmap or NULL.
-- 
-- The bitmap should be freed by a corresponding call to hwloc_bitmap_free().
foreign import ccall "hwloc_bitmap_alloc" allocate :: IO Bitmap


-- | Allocate a new full bitmap
foreign import ccall "hwloc_bitmap_alloc_full" allocateFull :: IO Bitmap

-- | Free bitmap
-- 
-- If bitmap is NULL, no operation is performed.
foreign import ccall "hwloc_bitmap_free" free :: Bitmap -> IO ()

-- | Duplicate a bitmap by allocating a new bitmap and copying bitmap contents.
--
-- If bitmap is NULL, NULL is returned.
--
foreign import ccall "hwloc_bitmap_dup" dup :: Bitmap -> IO Bitmap

-- | Copy the contents of bitmap src into the already allocated bitmap dst
foreign import ccall "hwloc_bitmap_copy" copy :: Bitmap -> Bitmap -> IO ()


-- | Stringify a bitmap.
-- 
-- Up to buflen characters may be written in buffer buf.
-- 
-- If buflen is 0, buf may safely be NULL.
-- 
-- Return the number of characters that were actually written if not truncating,
-- or that would have been written (not including the ending \0).
-- 
foreign import ccall "hwloc_bitmap_snprintf" snprintf :: Ptr a -> CSize -> Bitmap -> IO Int

-- | Stringify a bitmap into a newly allocated string.
foreign import ccall "hwloc_bitmap_asprintf" asprintf :: Ptr CString -> Bitmap -> IO Int

-- | Parse a bitmap string and stores it in bitmap.
foreign import ccall "hwloc_bitmap_sscanf" sscanf :: Bitmap -> CString -> IO Int

-- | Stringify a bitmap in the list format.
-- 
-- Lists are comma-separated indexes or ranges.
-- Ranges are dash separated indexes.
-- The last range may not have a ending indexes if the bitmap is infinite.
-- 
-- Up to \p buflen characters may be written in buffer \p buf.
-- 
-- If \p buflen is 0, \p buf may safely be \c NULL.
-- 
-- \return the number of character that were actually written if not truncating,
-- or that would have been written (not including the ending \\0).
-- 
foreign import ccall "hwloc_bitmap_list_snprintf" snprintfList :: Ptr a -> CSize -> Bitmap -> IO Int

-- | Stringify a bitmap into a newly allocated list string.
foreign import ccall "hwloc_bitmap_list_asprintf" asprintfList :: Ptr CString -> Bitmap -> IO Int

-- | Parse a list string and stores it in bitmap
foreign import ccall "hwloc_bitmap_list_sscanf" sscanfList :: Bitmap -> CString -> IO ()

-- | Stringify a bitmap in the taskset-specific format.
-- 
-- The taskset command manipulates bitmap strings that contain a single
-- (possible very long) hexadecimal number starting with 0x.
-- 
-- Up to \p buflen characters may be written in buffer \p buf.
-- 
-- If \p buflen is 0, \p buf may safely be \c NULL.
-- 
-- Return the number of character that were actually written if not truncating,
-- or that would have been written (not including the ending \\0).
-- 
foreign import ccall "hwloc_bitmap_taskset_snprintf" snprintfTaskset :: CString -> CSize -> Bitmap -> IO Int

-- | Stringify a bitmap into a newly allocated taskset-specific string.
foreign import ccall "hwloc_bitmap_taskset_asprintf" asprintfTaskset :: Ptr CString -> Bitmap -> IO Int

-- | Parse a taskset-specific bitmap string and stores it in bitmap \p bitmap.
foreign import ccall "hwloc_bitmap_taskset_sscanf" sscanfTaskset :: Bitmap -> CString -> IO Int


-- | Empty the bitmap
foreign import ccall "hwloc_bitmap_zero" zero :: Bitmap -> IO ()

-- | Fill bitmap with all possible indexes (even if those objects don't exist or are otherwise unavailable)
foreign import ccall "hwloc_bitmap_fill" fill :: Bitmap -> IO ()

-- | Empty the bitmap \p bitmap and add bit \p id
foreign import ccall "hwloc_bitmap_only" singleton :: Bitmap -> Word -> IO ()

-- | Fill the bitmap \p and clear the index \p id
foreign import ccall "hwloc_bitmap_allbut" allBut :: Bitmap -> Word -> IO ()

-- | Setup bitmap \p bitmap from unsigned long \p mask
foreign import ccall "hwloc_bitmap_from_ulong" setup :: Bitmap -> Word64 -> IO ()

-- | Setup bitmap \p bitmap from unsigned long \p mask used as \p i -th subset
foreign import ccall "hwloc_bitmap_from_ith_ulong" setupWithMask :: Bitmap -> Word64 -> Word64 -> IO ()


-- | Add index \p id in bitmap \p bitmap
foreign import ccall "hwloc_bitmap_set" setBit :: Bitmap -> Word -> IO ()

-- | Add indexes from \p begin to \p end in bitmap \p bitmap.
-- 
-- If \p end is \c -1, the range is infinite.
-- 
foreign import ccall "hwloc_bitmap_set_range" setRange :: Bitmap -> Word -> Int -> IO ()

-- | Replace \p i -th subset of bitmap \p bitmap with unsigned long \p mask
foreign import ccall "hwloc_bitmap_set_ith_ulong" setSubset :: Bitmap -> Word -> Word64 -> IO ()

-- | Remove index \p id from bitmap \p bitmap
foreign import ccall "hwloc_bitmap_clr" clearBit :: Bitmap -> Word -> IO ()

-- | Remove indexes from \p begin to \p end in bitmap \p bitmap.
-- 
-- If \p end is \c -1, the range is infinite.
-- 
foreign import ccall "hwloc_bitmap_clr_range" clearRange :: Bitmap -> Word -> Int -> IO ()

-- | Keep a single index among those set in bitmap \p bitmap
-- 
-- May be useful before binding so that the process does not have a chance of
-- migrating between multiple logical CPUs in the original mask.
foreign import ccall "hwloc_bitmap_singlify" singlify :: Bitmap -> IO ()


-- | Convert the beginning part of bitmap \p bitmap into unsigned long \p mask
foreign import ccall "hwloc_bitmap_to_ulong" getFirstBits :: Bitmap -> IO Word64

-- | Convert the \p i -th subset of bitmap \p bitmap into unsigned long mask
foreign import ccall "hwloc_bitmap_to_ith_ulong" getSubset :: Bitmap -> Word -> IO Word64

-- | Test whether index \p id is part of bitmap \p bitmap
foreign import ccall "hwloc_bitmap_isset" testBit :: Bitmap -> Word -> IO Int

-- | Test whether bitmap \p bitmap is empty
foreign import ccall "hwloc_bitmap_iszero" null :: Bitmap -> IO Int

-- | Test whether bitmap \p bitmap is completely full
foreign import ccall "hwloc_bitmap_isfull" full :: Bitmap -> IO Int

-- | Compute the first index (least significant bit) in bitmap \p bitmap
-- 
-- Return -1 if no index is set.
-- 
foreign import ccall "hwloc_bitmap_first" getFirstBit :: Bitmap -> IO Int

-- | Compute the next index in bitmap \p bitmap which is after index \p prev
-- 
-- If \p prev is -1, the first index is returned.
-- 
-- Return -1 if no index with higher index is bitmap.
-- 
foreign import ccall "hwloc_bitmap_next" getBitAfter :: Bitmap -> Int -> IO Int

-- | Compute the last index (most significant bit) in bitmap \p bitmap
-- 
-- Return -1 if no index is bitmap, or if the index bitmap is infinite.
-- 
foreign import ccall "hwloc_bitmap_last" getLastBit :: Bitmap -> IO Int

-- | Compute the "weight" of bitmap (i.e., number of indexes that are in the
-- bitmap).
-- 
-- Return the number of indexes that are in the bitmap.
foreign import ccall "hwloc_bitmap_weight" getWeight :: Bitmap -> IO Int


-- | Or bitmaps \p bitmap1 and \p bitmap2 and store the result in bitmap \p res
-- 
-- \p res can be the same as \p bitmap1 or \p bitmap2
-- 
foreign import ccall "hwloc_bitmap_or" union :: Bitmap -> Bitmap -> Bitmap -> IO ()

-- | And bitmaps \p bitmap1 and \p bitmap2 and store the result in bitmap \p res
-- 
-- \p res can be the same as \p bitmap1 or \p bitmap2
-- 
foreign import ccall "hwloc_bitmap_and" intersection :: Bitmap -> Bitmap -> Bitmap -> IO ()

-- | And bitmap \p bitmap1 and the negation of \p bitmap2 and store the result in bitmap \p res
-- 
-- \p res can be the same as \p bitmap1 or \p bitmap2
-- 
foreign import ccall "hwloc_bitmap_andnot" difference :: Bitmap -> Bitmap -> Bitmap -> IO ()

-- | Xor bitmaps \p bitmap1 and \p bitmap2 and store the result in bitmap \p res
-- 
-- \p res can be the same as \p bitmap1 or \p bitmap2
-- 
foreign import ccall "hwloc_bitmap_xor" symetricDifference :: Bitmap -> Bitmap -> Bitmap -> IO ()

-- | Negate bitmap \p bitmap and store the result in bitmap \p res
-- 
-- \p res can be the same as \p bitmap
-- 
foreign import ccall "hwloc_bitmap_not" negate :: Bitmap -> Bitmap -> IO ()


-- | Test whether bitmaps \p bitmap1 and \p bitmap2 intersects
foreign import ccall "hwloc_bitmap_intersects" intersects :: Bitmap -> Bitmap -> IO Int

-- | Test whether bitmap \p sub_bitmap is part of bitmap \p super_bitmap
foreign import ccall "hwloc_bitmap_isincluded" isSubsetOf :: Bitmap -> Bitmap -> IO Int

-- | Test whether bitmap \p bitmap1 is equal to bitmap \p bitmap2
foreign import ccall "hwloc_bitmap_isequal" isEqualTo :: Bitmap -> Bitmap -> IO Int

-- | Compare bitmaps \p bitmap1 and \p bitmap2 using their lowest index.
-- 
-- Smaller least significant bit is smaller.
-- The empty bitmap is considered higher than anything.
-- 
foreign import ccall "hwloc_bitmap_compare_first" compareFirst :: Bitmap -> Bitmap -> IO Int

-- | Compare bitmaps \p bitmap1 and \p bitmap2 in lexicographic order.
-- 
-- Lexicographic comparison of bitmaps, starting for their highest indexes.
-- Compare last indexes first, then second, etc.
-- The empty bitmap is considered lower than anything.
-- 
-- \note This is different from the non-existing hwloc_bitmap_compare_last()
-- which would only compare the highest index of each bitmap.
-- 
foreign import ccall "hwloc_bitmap_compare" compare :: Bitmap -> Bitmap -> IO Int

