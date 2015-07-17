
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Bits
import Data.List (foldl')
import Data.Int (Int64)

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as BC
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array
import System.IO.Unsafe
import Foreign.Storable
import Data.Word

type Digest = ByteString

main = do
  a <- B.getContents
  putStrLn (stringDigest (md5 a))

md5 :: ByteString -> Digest
md5 bs = let initialState = (0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476)
             (a, b, c, d) = foldl' md5Update initialState (blk 64 False bs)
          in wordsToBytes [a,b,c,d]
  where
    md5Update (a,b,c,d) w =
      let ws = cycle (bytesToWords w)
          ma = [ floor (0x100000000 * abs ( sin x)) | x <- [1..64::Double]]
          m1 = take 16 ma
          m2 = take 16 (drop 16 ma)
          m3 = take 16 (drop 32 ma)
          m4 = take 16 (drop 48 ma)
          (z1,z2,z3,z4) = foldl ch2 (a,b,c,d) [
             (\x y z -> z `xor` (x .&. (y `xor` z)) , ws,                     [ 7, 12, 17, 22], m1),
             (\x y z -> y `xor` (z .&. (x `xor` y)) , everynth 4 (tail ws),   [ 5,  9, 14, 20], m2),
             (\x y z -> x `xor` y `xor` z ,           everynth 2 (drop 5 ws), [ 4, 11, 16, 23], m3),
             (\x y z -> y `xor` (x .|. complement z), everynth 6 ws         , [ 6, 10, 15, 21], m4) ]
       in (z1+a, z2+b, z3+c, z4+d)
    mch fn h i j k x s ac = i + rotateL (fn i j k + (x + ac + h)) s
    ch1 fn (h,i,j,k) ws rs ms = let r = mch fn h i j k (head ws) (head rs) (head ms)
                 in r : (if null (tail ms) then [] else ch1 fn (k,r,i,j) (tail ws) (tail rs) (tail ms) )
    ch2 e (f,w1,r,m) = let [h,i,j,k] = drop 12 (ch1 f e w1 (cycle r) m) in (h,k,j,i)
    everynth n ys = head ys : everynth n (drop (n+1) ys )

    blk k se bs = chunkify bs 0
      where 
        chunkify :: ByteString -> Int64 -> [ByteString] 
        chunkify x n = if B.length x >= k then B.take k x : chunkify (B.drop k x) (n+fromIntegral k)
                           else let ln = fromIntegral $ B.length x
                                    tpad = k - fromIntegral ln
                                    kx = 1 + (k `div` 8)
                                    pad = if tpad < kx then tpad + k else tpad
                                    xbs = B.concat [x, B.singleton 0x80, B.replicate (pad - kx) 0, bc ]
                                    sdp = fromIntegral (8 * (n + ln ))
                                    sdc = if se then swapEndian64 sdp else sdp
                                    bc = longsToBytes ( (if k == 128 then [0] else [] ) ++ [ sdc ] )
                                 in if tpad < kx then [B.take k xbs, B.drop k xbs] else [xbs]
    


-- | An array of Word32 is copied byte by byte into a ByteString
wordsToBytes :: [Word32] -> B.ByteString
wordsToBytes a = let n = length a
                     d = undefined :: Word32
                  in B.unsafeCreate (sizeOf d * n) $ \p -> pokeArray (castPtr p) a

-- | A ByteString is copied byte for byte where every 4 bytes is treated as a Word32
bytesToWords :: B.ByteString -> [Word32]
bytesToWords a = let (p, o, n) = B.toForeignPtr a
                     d = undefined :: Word32
                     c = n `div` (sizeOf d) :: Int
                  in unsafeDupablePerformIO $ withForeignPtr p $ \px -> peekArray c (plusPtr px o)

-- | An array of Word64 is copied byte by byte into a ByteString
longsToBytes :: [Word64] -> B.ByteString
longsToBytes a = let n = length a
                     d = undefined :: Word64
                  in B.unsafeCreate (sizeOf d * n) $ \p -> pokeArray (castPtr p) a

-- | A Word64 has its bytes swapped resulting in the Word64 which would have been
-- represented by the opposite endian-ness.
swapEndian64 :: Word64 -> Word64
swapEndian64 x = (shiftR x 56) .|. ((x .&. 0x00FF000000000000) `shiftR` 40)
   .|. ((x .&. 0x0000FF0000000000) `shiftR` 24) .|. ((x .&. 0x000000FF00000000) `shiftR` 8)
   .|. ((x .&. 0x00000000FF000000) `shiftL` 8) .|. ((x .&. 0x0000000000FF0000) `shiftL` 24)
   .|. ((x .&. 0x000000000000FF00) `shiftL` 40) .|. (shiftL x 56)

-- should this be base16 ?
stringDigest :: Digest -> String
stringDigest = concatMap shex . B.unpack 
  where shex :: Integral a => a -> String
        shex n = let (a,b) = divMod (fromIntegral n) 16 in [BC.index chars a, BC.index chars b]
        chars = BC.pack ("0123456789abcdef" :: String)
