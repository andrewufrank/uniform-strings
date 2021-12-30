---------------------------------------------------------------------
--
-- Module      :  Strings
-- Copyright   :
--
-- | a module to convert between character string encodings
--

-- would require systematic checks what are permited characters
-- (especially for input to urlEncoding)

-- the latin encoding is produced by show ...
-- t2u is nearly invertible...

-- strings remain here, to be used when constructing the wrappers for
-- functions used from other packages (with String interfaces)
----------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving
    , DeriveGeneric
    , DeriveAnyClass
    , TypeSynonymInstances
      #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Strings.Conversion (
    ByteString, LazyByteString
    , s2b, b2s, b2t,   t2b, t2u,  s2u
    , s2t, t2s
    , t2tl, tl2t
    -- uses UTF8 as encoding in ByteString
    -- urlencode is always represented the same as the input
    , Text (..), BSUTF (..), URL (..)
    -- , URLform , b2uf, b2urlf, urlf2b,

    , b2bu, bu2b, bu2s, bu2t, t2bu, s2bu
    , u2b, u2t,  u2s, b2u
    , b2bl, bl2b -- lazy bytestring
    , bl2t, t2bl
    , bb2t, bb2s  -- conversion with error if not UTF8
    , s2latin, t2latin, latin2t, latin2s -- conversion to the latin1 encoding
    , BSlat (..), s2lat, lat2s, t2lat, lat2t
    , s3lat, t3lat, s3latin, t3latin
    , s2url, url2s,  unURL, t22latin
    , convertLatin, findNonLatinChars, findNonLatinCharsT
    , filterLatin
    , module Safe
    )   where
-- 
import           Safe (fromJustNote)
import GHC.Generics (Generic)
import Uniform.Zero (Zeros(zero) )

import Control.Monad (join)

import           Data.Text            (Text)
import qualified Data.Text            as T
import Data.Char (ord)
import Data.List (nub)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as Lazy 
import Data.ByteString.Char8 (pack, unpack)

import           Data.Text.Encoding   (decodeUtf8, decodeUtf8', encodeUtf8)

import qualified Network.URI          as URI
-- import qualified Snap.Core            as SN

import qualified Data.Text.Lazy as LText  -- (toStrict, fromStrict)

bl2t :: LazyByteString ->Text
-- ^ conversion from LazyByteString to text (only if guarantee that only utf8 values)
bl2t =    bu2t . BSUTF . bl2b

t2bl :: Text -> LazyByteString
t2bl =   b2bl . t2b


s2t :: String -> Text
-- ^ String to Text (invertable)
s2t = T.pack

t2s :: Text -> String
-- ^ String to Text (invertable)
t2s = T.unpack

tl2t :: LText.Text -> Text
tl2t = LText.toStrict

t2tl :: Text -> LText.Text
t2tl = LText.fromStrict

type LazyByteString = Lazy.ByteString

instance Zeros ByteString where zero = t2b ""
instance Zeros LazyByteString where zero = b2bl zero

newtype BSUTF = BSUTF ByteString
    deriving (Show, Read, Eq, Ord, Generic, Zeros, Semigroup, Monoid)

unBSUTF :: BSUTF -> ByteString
unBSUTF (BSUTF a) = a


t2bu :: Text ->  BSUTF
-- ^ Text to Bytestring (invertable)
t2bu = BSUTF . encodeUtf8

bu2t ::  BSUTF -> Text
-- ^ ByteString to Text --  inverse (not an arbitrary input)
bu2t = decodeUtf8 . unBSUTF


-- conversion ByteString BSUTF
b2bu :: ByteString -> Maybe BSUTF
b2bu a = if testByteStringUtf8 a then Just (BSUTF a) else Nothing

bu2b :: BSUTF -> ByteString
bu2b = unBSUTF

bu2s :: BSUTF -> String
bu2s = t2s . bu2t

testByteStringUtf8 :: ByteString -> Bool
-- ^ test whether a byte string is valid utf8 encoded
-- used for avoiding problems with the quickcheck conversions
testByteStringUtf8 b =
    case decodeUtf8' b of
                -- :: ByteString -> Either UnicodeException Text
                    Left s  -> False
                    Right t -> True

t2b :: Text -> ByteString
t2b = bu2b . t2bu

b2t :: ByteString -> Maybe Text
b2t = fmap bu2t . b2bu

bb2s :: ByteString -> String
-- converts and stops with error when not UTF8
bb2s s = fromJustNote ("bb2s - bytestring to string conversion: " ++ show s
        ++ " was not a utf8") . b2s $ s

bb2t :: ByteString -> Text
-- converts and stopw with error when not UTF8
bb2t s = fromJustNote ("bb2s - bytestring to text conversion: " ++ show s
        ++ " was not a utf8") . b2t $ s
-- bytestring -- string (just a composition of t2s . b2t and reverse)
s2bu :: String ->  BSUTF
-- ^ String to Bytestring (invertable)
s2bu = BSUTF . encodeUtf8 . s2t

--bu2s ::  BSUTF -> String
---- ^ ByteString to String -- not inverse (not any arbitrary input)
--bu2s = t2s . decodeUtf8 . unBSUTF

s2b :: String -> ByteString
s2b = t2b . s2t

b2bl :: ByteString -> Lazy.ByteString
b2bl = Lazy.fromStrict

bl2b ::  Lazy.ByteString -> ByteString
bl2b = Lazy.toStrict

b2s :: ByteString -> Maybe String
b2s = fmap t2s . b2t


newtype URL = URL String deriving (Show, Eq)
instance Zeros URL where zero = URL zero

unURL :: URL -> String
unURL (URL t) = t


s2url :: String -> URL
-- ^ convert string to url   (uses code from Network.HTTP, which converts space into %20)
s2url =   URL . URI.escapeURIString URI.isUnescapedInURIComponent
--s2url =   URL . HTTP.urlEncode

url2s :: URL -> String
-- ^ convert url to string   (uses code from Network.HTTP, which converts space into %20)
url2s  =   URI.unEscapeString . unURL

testUrlEncodingURI :: String -> Bool
testUrlEncodingURI a = a == (unURL . s2url . url2s . URL $ a)

url2u :: URL -> String
url2u = unURL
u2url :: String -> Maybe URL
u2url a = if testUrlEncodingURI a then Just (URL a) else Nothing


s2u :: String -> String
-- ^ convert string to url   (uses code from Network.HTTP, which converts space into %20)
s2u = url2u . s2url

u2s :: String -> Maybe String     --not inverse
-- ^ convert url to string   (uses code from Network.HTTP, which converts space into %20)
u2s  =   fmap url2s . u2url


-- case for encoding of form content (with + for space)
-- removed for 9.2.1 
-- newtype URLform = URLform ByteString deriving (Show, Eq)
-- unURLform :: URLform -> ByteString
-- unURLform (URLform t) = t


-- b2urlf :: ByteString -> URLform
-- -- ^ convert string to url   (uses code from SNAP, which converts space into +)
-- b2urlf =   URLform . SN.urlEncode

-- urlf2b :: URLform -> ByteString
-- -- ^ convert url to string   (uses code from SNAP, which converts space into +)
-- urlf2b = fromJustNote "urlf2b nothing" . SN.urlDecode . unURLform


-- testUrlEncodingSNAP :: ByteString -> Bool
-- testUrlEncodingSNAP a =  maybe False ((a ==). SN.urlEncode) . SN.urlDecode $ a
 
-- urlf2u :: URLform -> ByteString
-- urlf2u = unURLform
-- u2urlf :: ByteString -> Maybe URLform
-- u2urlf a = if testUrlEncodingSNAP a then Just (URLform a) else Nothing
-- -- this test allows control in url encoded strings ...


-- b2uf :: ByteString -> ByteString
-- -- ^ convert ByteString to url   (uses code from SNAP which converts space into +)
-- b2uf = urlf2u . b2urlf

-- uf2b :: ByteString -> Maybe ByteString     --not inverse
-- -- ^ convert url to ByteString   (uses code from SNAP, which converts space into +)
-- uf2b  =   fmap urlf2b . u2urlf


t2u :: Text -> Text
t2u = s2t . s2u . t2s
u2t :: Text -> Maybe Text
u2t = fmap s2t . u2s . t2s

b2u :: ByteString -> Maybe ByteString
b2u = fmap (s2b . s2u) . b2s
u2b :: ByteString -> Maybe ByteString
u2b = fmap s2b . join  . fmap u2s . b2s


-- | bytestring with latin1 encoded characters
newtype BSlat = BSlat ByteString deriving (Show, Eq)
unBSlat :: BSlat -> ByteString
unBSlat (BSlat a) = a


lat2s :: BSlat -> String
-- ^ bytestring with latin encoding to string
lat2s = latin2s . unBSlat

s2lat :: String -> Maybe BSlat   -- is this always possible ?
-- ^ string encoded as ByteString with latin encoding, if possible
s2lat =  fmap BSlat . s22latin

s3lat :: String ->  BSlat   -- is this always possible ?
-- ^ string converted to represenatable as latin and then encoded
-- lossy!
s3lat   =  BSlat . s3latin

lat2t :: BSlat -> Text
-- ^ Text encoded as ByteString with latin encoding, if possible
lat2t = latin2t . unBSlat

t2lat :: Text -> Maybe BSlat   -- is this always possible
-- ^ Text encoded as ByteString with latin encoding, if possible
t2lat = fmap BSlat . t22latin

t3lat :: Text -> BSlat   -- is this always possible
-- ^ Text converted to represenatable as latin and then encoded
-- lossy!
t3lat =  BSlat . t3latin

latin2s :: ByteString -> String
    --    | works always, but produces unexpected results if bytestring is not latin encoded
latin2s = Data.ByteString.Char8.unpack
--
s2latin :: String ->  ByteString
        --  | works always, but produces unexpected results if bytestring is not latin encoded
s2latin =  Data.ByteString.Char8.pack

s22latin :: String -> Maybe ByteString
s22latin s = if all  ((<256) . ord) s  then  Just .  s2latin  $ s else Nothing   -- Data.ByteString.Char8.pack . T.unpack

s3latin :: String ->  ByteString
s3latin =   s2latin  . convertLatin

filterLatin :: String -> String
filterLatin = filter ((<256).ord )

convertLatin :: String -> String
-- ^ convert a string to contain only characters in latin1
convertLatin = map conv2latinChar

conv2latinChar :: Char -> Char
-- ^ convert character not in the latin1 encoding (intelligently treating quotes and double quotes)
-- possibly other cases later added
conv2latinChar c = if ord c < 256 then c else
    case c of
        '\x201C' -> '"'
        '\x201D' -> '"'
        '\x201E' -> '"'
        '\8212' -> '-'    -- em dash
        '\8222' -> '"'    -- lower quote
        '\8216' -> '\''    --  left single quote
        '\8217' -> '\''    -- right single quote
        '\8218' -> '\''    --  quote
        '\8221' -> '"'    -- unclear why 8221 but is quote
--        '\x2018' -> '\''   -- same as 8216
--        '\x2019' -> '\''  -- same as 8217

        _ -> c -- '\SUB'    -- could be another char ? \SUB

findNonLatinChars :: String -> String
-- ^ the result is a string of all the characters not in the latin1 encoding
-- possibly apply conv2latinChar first
findNonLatinChars = nub . filter ((>256).ord )
--            (\c -> conv2latinChar c == '\SUB')

findNonLatinCharsT :: Text -> Text
-- ^ the result is a string of all the characters not in the latin1 encoding
findNonLatinCharsT = s2t . findNonLatinChars . t2s



latin2t :: ByteString -> Text
latin2t = s2t . latin2s  -- T.pack .  Data.ByteString.Char8.unpack

t2latin :: Text ->  ByteString
-- text to bytestring -  works always, but produces unexpected results if bytestring is not latin encoded
t2latin   = s2latin . t2s  -- Data.ByteString.Char8.pack . T.unpack

t22latin :: Text -> Maybe ByteString
-- ^ converts text to bytestring, if meaningful
t22latin   = s22latin . t2s    -- Data.ByteString.Char8.pack . T.unpack
--t22latin t = if all  ((<256) . ord) (t2s t) then  Just .  s2latin . t2s $ t else Nothing   -- Data.ByteString.Char8.pack . T.unpack

t3latin :: Text ->  ByteString
-- text to bytestring - meaningful, but converted -- lossy!
t3latin   = s3latin . t2s  -- Data.ByteString.Char8.pack . T.unpack
--

putIOwords :: [Text] -> IO ()
putIOwords = putStrLn . unlines . map t2s

-- chars :: [GHC.Word.Word8]
chars = [198, 216, 197, 206, 219,140,252,202, 419, 420, 1937 ]
difficultBString = ByteString.pack chars
difficultTString = "\198\216\197\206\219\140\252\202\419\420\1937"

