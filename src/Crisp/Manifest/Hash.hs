{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.Manifest.Hash
-- Description : Content hashing for Crisp manifests
--
-- Provides SHA-256 content hashing for:
-- - Text content
-- - ByteString content (files)
-- - Hash formatting and parsing

module Crisp.Manifest.Hash
  ( -- * Hashing Functions
    hashContent
  , hashByteString
  , hashFile
    -- * Hash Formatting
  , formatHash
  , parseHash
    -- * Hash Verification
  , verifyHash
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word8, Word32, Word64)
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR, rotateR, complement)
import Numeric (showHex)
import Data.Char (digitToInt, isHexDigit)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

-- | Compute SHA-256 hash of text content
hashContent :: Text -> Text
hashContent = hashByteString . TE.encodeUtf8

-- | Compute SHA-256 hash of ByteString content
hashByteString :: ByteString -> Text
hashByteString = bytesToHex . sha256

-- | Compute SHA-256 hash of a file (placeholder - would use IO)
hashFile :: FilePath -> IO Text
hashFile path = do
  content <- BS.readFile path
  pure $ hashByteString content

-- | Format a hash with the sha256: prefix
formatHash :: Text -> Text
formatHash hash = "sha256:" <> hash

-- | Parse a hash, removing the sha256: prefix if present
parseHash :: Text -> Maybe Text
parseHash txt
  | "sha256:" `T.isPrefixOf` txt = Just (T.drop 7 txt)
  | T.length txt == 64 && T.all isHexDigit txt = Just txt
  | otherwise = Nothing

-- | Verify that content matches a given hash
verifyHash :: Text -> Text -> Bool
verifyHash content expectedHash =
  let actualHash = hashContent content
  in actualHash == expectedHash || ("sha256:" <> actualHash) == expectedHash

--------------------------------------------------------------------------------
-- SHA-256 Implementation
--------------------------------------------------------------------------------

-- | SHA-256 hash computation
sha256 :: ByteString -> ByteString
sha256 msg =
  let paddedMsg = padMessage msg
      blocks = chunksOf 64 paddedMsg
      initialHash = (h0, h1, h2, h3, h4, h5, h6, h7)
      finalHash = foldl processBlock initialHash blocks
  in hashToBytes finalHash

-- Initial hash values (first 32 bits of fractional parts of square roots of first 8 primes)
h0, h1, h2, h3, h4, h5, h6, h7 :: Word32
h0 = 0x6a09e667
h1 = 0xbb67ae85
h2 = 0x3c6ef372
h3 = 0xa54ff53a
h4 = 0x510e527f
h5 = 0x9b05688c
h6 = 0x1f83d9ab
h7 = 0x5be0cd19

-- Round constants (first 32 bits of fractional parts of cube roots of first 64 primes)
kConstants :: [Word32]
kConstants =
  [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
  , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
  , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
  , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
  , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
  , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
  , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
  , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  ]

-- | Pad message to multiple of 512 bits (64 bytes)
padMessage :: ByteString -> ByteString
padMessage msg =
  let msgLen = BS.length msg
      -- Add 1 bit (0x80 byte) then zeros, then 64-bit length
      bitLen = fromIntegral msgLen * 8 :: Word64
      -- Padding to make (msgLen + 1 + padding + 8) mod 64 == 0
      padLen = (55 - msgLen) `mod` 64
      padding = BS.singleton 0x80 <> BS.replicate padLen 0x00
      lenBytes = word64ToBytes bitLen
  in msg <> padding <> lenBytes

-- | Process a 512-bit (64-byte) block
processBlock :: (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32)
             -> ByteString
             -> (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32)
processBlock (a0, b0, c0, d0, e0, f0, g0, h0') block =
  let -- Parse 16 32-bit words from block
      w0to15 = [bytesToWord32 (BS.take 4 (BS.drop (i*4) block)) | i <- [0..15]]

      -- Extend to 64 words
      w = extendSchedule w0to15

      -- 64 rounds of compression
      (a, b, c, d, e, f, g, h) = foldl (compressionRound w) (a0, b0, c0, d0, e0, f0, g0, h0') [0..63]

  in (a0 + a, b0 + b, c0 + c, d0 + d, e0 + e, f0 + f, g0 + g, h0' + h)

-- | Extend message schedule from 16 to 64 words
extendSchedule :: [Word32] -> [Word32]
extendSchedule w0to15 = go (reverse w0to15) 16
  where
    go ws 64 = reverse ws
    go ws i =
      let w = ws
          w_i_2  = w !! 1
          w_i_7  = w !! 6
          w_i_15 = w !! 14
          w_i_16 = w !! 15
          s0 = rotateR w_i_15 7 `xor` rotateR w_i_15 18 `xor` shiftR w_i_15 3
          s1 = rotateR w_i_2 17 `xor` rotateR w_i_2 19 `xor` shiftR w_i_2 10
          newW = w_i_16 + s0 + w_i_7 + s1
      in go (newW : ws) (i + 1)

-- | Single compression round
compressionRound :: [Word32]
                 -> (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32)
                 -> Int
                 -> (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32)
compressionRound w (a, b, c, d, e, f, g, h) i =
  let s1 = rotateR e 6 `xor` rotateR e 11 `xor` rotateR e 25
      ch = (e .&. f) `xor` (complement e .&. g)
      temp1 = h + s1 + ch + (kConstants !! i) + (w !! i)
      s0 = rotateR a 2 `xor` rotateR a 13 `xor` rotateR a 22
      maj = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
      temp2 = s0 + maj
  in (temp1 + temp2, a, b, c, d + temp1, e, f, g)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Convert hash state to ByteString
hashToBytes :: (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32) -> ByteString
hashToBytes (a, b, c, d, e, f, g, h) =
  BS.concat $ map word32ToBytes [a, b, c, d, e, f, g, h]

-- | Convert Word32 to big-endian ByteString
word32ToBytes :: Word32 -> ByteString
word32ToBytes w = BS.pack
  [ fromIntegral (shiftR w 24)
  , fromIntegral (shiftR w 16)
  , fromIntegral (shiftR w 8)
  , fromIntegral w
  ]

-- | Convert Word64 to big-endian ByteString
word64ToBytes :: Word64 -> ByteString
word64ToBytes w = BS.pack
  [ fromIntegral (shiftR w 56)
  , fromIntegral (shiftR w 48)
  , fromIntegral (shiftR w 40)
  , fromIntegral (shiftR w 32)
  , fromIntegral (shiftR w 24)
  , fromIntegral (shiftR w 16)
  , fromIntegral (shiftR w 8)
  , fromIntegral w
  ]

-- | Convert big-endian ByteString to Word32
bytesToWord32 :: ByteString -> Word32
bytesToWord32 bs =
  let [b0, b1, b2, b3] = map fromIntegral (BS.unpack (BS.take 4 bs))
  in shiftL b0 24 .|. shiftL b1 16 .|. shiftL b2 8 .|. b3

-- | Convert ByteString to hex Text
bytesToHex :: ByteString -> Text
bytesToHex = T.pack . concatMap byteToHex . BS.unpack
  where
    byteToHex b =
      let (hi, lo) = (b `shiftR` 4, b .&. 0x0f)
      in [hexChar hi, hexChar lo]
    hexChar n
      | n < 10 = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')

-- | Split ByteString into chunks
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n bs
  | BS.null bs = []
  | otherwise = BS.take n bs : chunksOf n (BS.drop n bs)
