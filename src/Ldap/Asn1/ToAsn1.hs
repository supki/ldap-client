{-# LANGUAGE CPP #-}
module Ldap.Asn1.ToAsn1
  ( Ber
  , encode
  , bool
  , int32
  , enum
  , octetstring
  , null
  , sequence
  , set
  , tagged
  , Mod
  , Tag
  , application
  , context
  , tag
  ) where

import           Data.Bits (Bits((.&.), (.|.), shiftR))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as Builder
import           Data.Int (Int64, Int32)
import           Data.List.NonEmpty (NonEmpty((:|)))
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
#endif
import           Data.Semigroup (Semigroup(..))
import           Data.Word (Word8)
import           Prelude hiding (null, sequence)

-- $setup
-- >>> :set -XOverloadedStrings

data Ber = Ber !Int64 !Builder

instance Semigroup Ber where
  Ber l b <> Ber l' b' = Ber (l + l') (b <> b')

instance Monoid Ber where
  mempty = Ber 0 mempty
  mappend = (<>)

encode :: Ber -> ByteString
encode (Ber _ b) = Builder.toLazyByteString b

-- | Encoding of a boolean value.
--
-- >>> encode (bool mempty True)
-- "\SOH\SOH\255"
--
-- >>> encode (bool mempty False)
-- "\SOH\SOH\NUL"
bool :: Mod -> Bool -> Ber
bool f b = fromBytes ((t .|. classBit f) : ts ++ [0x01, if b then 0xFF else 0x00])
 where
  t :| ts = tagBits (tag 0x01 <> f)

-- | Encoding of an integer value.
--
-- >>> encode (int32 mempty 0)
-- "\STX\SOH\NUL"
--
-- >>> encode (int32 mempty 127)
-- "\STX\SOH\DEL"
--
-- >>> encode (int32 mempty 128)
-- "\STX\STX\NUL\128"
int32 :: Mod -> Int32 -> Ber
int32 f n = fromBytes ((t .|. classBit f) : ts ++ fromIntegral (length bytes) : bytes)
 where
  t :| ts = tagBits (tag 0x02 <> f)
  bytes
    | n .&. 0x80 == 0x80 = 0x00 : reverse (go n)
    | otherwise = reverse (go n)
   where
    go i
      | i <= 0xff = return (fromIntegral i)
      | otherwise = (fromIntegral i .&. 0xff) : go (i `shiftR` 8)

-- | Encoding of an enumerated value.
--
-- It is encoded exactly the same as an integer value, but the tag number is different.
enum :: Mod -> Int32 -> Ber
enum f = int32 (tag 0x0a <> f)

-- | Encoding of an octet string.
octetstring :: Mod -> ByteString.ByteString -> Ber
octetstring f bs = Ber
  (fromIntegral (ByteString.length bs) + 2 + fromIntegral (length ts))
  (Builder.word8 (t .|. classBit f) <> Builder.lazyByteString (ByteString.Lazy.pack ts) <>
   Builder.byteString (ByteString.pack (encodeLength (ByteString.length bs))) <>
   Builder.byteString bs)
 where
  t :| ts = tagBits (tag 0x04 <> f)

-- | Encoding of NULL
--
-- >>> encode (null mempty)
-- "\ENQ\NUL"
null :: Mod -> Ber
null f = fromBytes ((t .|. classBit f) : ts ++ [0])
 where
  t :| ts = tagBits (tag 0x05 <> f)

-- | Encoding of a sequence [of].
--
-- >>> encode (sequence mempty (octetstring mempty "Smith" <> bool mempty True))
-- "0\n\EOT\ENQSmith\SOH\SOH\255"
sequence :: Mod -> Ber -> Ber
sequence m = tagged (tag 0x10 <> m)

-- | Encoding of a set [of].
--
-- >>> encode (set mempty (octetstring mempty "Smith" <> bool mempty True))
-- "1\n\EOT\ENQSmith\SOH\SOH\255"
set :: Mod -> Ber -> Ber
set m = tagged (tag 0x11 <> m)

-- | Encoding of a (possibly tagged) constructed value.
tagged :: Mod -> Ber -> Ber
tagged f b@(Ber l _) = fromBytes ((t .|. constructedTag .|. classBit f) : ts ++ encodeLength l) <> b
 where
  t :| ts = tagBits f
  constructedTag = 0x20

fromBytes :: [Word8] -> Ber
fromBytes xs = let bs = ByteString.Lazy.pack xs in Ber (ByteString.Lazy.length bs) (Builder.lazyByteString bs)

defaultTag :: Tag
defaultTag = Tag Universal (Number 0)

newtype Mod = Mod (Tag -> Tag)

instance Semigroup Mod where
  Mod f <> Mod g = Mod (g . f)

instance Monoid Mod where
  mappend = (<>)
  mempty = Mod id

data Class =
    Universal
  | Application
  | Context
    deriving (Show, Eq)

data Tag = Tag !Class !Number
    deriving (Show, Eq)

newtype Number = Number Word8
    deriving (Show, Eq)

classBit :: Mod -> Word8
classBit (Mod f) = case f defaultTag of
  Tag Universal _   -> 0x00
  Tag Application _ -> 0x40
  Tag Context _     -> 0x80

tagBits :: Mod -> NonEmpty Word8
tagBits (Mod f) = case f defaultTag of Tag _ t -> encodeTagNumber t

application, context :: Mod
application = class_ Application
context     = class_ Context

class_ :: Class -> Mod
class_ c = Mod (\(Tag _ t) -> Tag c t)

tag :: Word8 -> Mod
tag t = Mod (\(Tag c _) -> Tag c (Number t))

-- | Small tag numbers (up to and including 30) are bit-OR'd
-- directly with the first Identifier byte, while the bigger ones
-- are encoded idiosyncratically.
--
-- >>> encodeTagNumber (Number 19)
-- 19 :| []
--
-- >>> encodeTagNumber (Number 31)
-- 31 :| [31]
--
-- >>> encodeTagNumber (Number 137)
-- 31 :| [129,9]
encodeTagNumber :: Number -> NonEmpty Word8
encodeTagNumber (Number n)
  | n < 30    = return n
  | otherwise = 0x1f :| reverse (go n)
 where
  go  x = fromIntegral (x .&. 0x7f) : go' (x `shiftR` 7)
  go' 0 = []
  go' x = (fromIntegral (x .&. 0x7f) .|. 0x80) : go' (x `shiftR` 7)

-- | Small lengths (up to and including 127) are returned as a single
-- byte equal to length itself, while the bigger one are encoded
-- idiosyncratically.
--
-- >>> encodeLength 7
-- [7]
--
-- >>> encodeLength 12238
-- [130,47,206]
--
-- @
-- encodeLength :: (Integral a, Bits a) => a -> NonEmpty Word8
-- @
encodeLength :: (Integral a, Bits a) => a -> [Word8]
encodeLength n
  | n < 0x80  = [fromIntegral n]
  | otherwise = let (l, xs) = go n in (l .|. 0x80) : reverse xs
 where
  go x
    | x <= 0xff = (1, [fromIntegral x])
    | otherwise = let (l, xs) = go (x `shiftR` 8) in (l + 1, (fromIntegral x .&. 0xff) : xs)
