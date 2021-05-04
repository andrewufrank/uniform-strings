-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
--{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving
--    , GeneralizedNewtypeDeriving
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
    , Text (..), BSUTF (..), URL (..), URLform

    , b2bu, bu2b, bu2s, bu2t, t2bu, s2bu
    , u2b, u2t, b2uf, u2s, b2u
    , b2bl, bl2b -- lazy bytestring
    , bl2t, t2bl 
    , bb2t, bb2s  -- conversion with error if not UTF8
    , s2latin, t2latin, latin2t, latin2s -- conversion to the latin1 encoding
    , BSlat (..), s2lat, lat2s, t2lat, lat2t
    , s3lat, t3lat, s3latin, t3latin
    , s2url, url2s, b2urlf, urlf2b, unURL, t22latin
    , convertLatin, findNonLatinChars, findNonLatinCharsT
    , filterLatin
    , module Safe
    )   where

import           Safe
import GHC.Generics
import Uniform.Zero
import Data.Semigroup

import Control.Monad (join)
--import Data.ByteString.Arbitrary
-- does not produce a simple Arbitrary for Bytestring, could be converted?


-- import "monads-tf" Control.Monad.State (MonadIO, liftIO)

-- import GHC.Exts( IsString(..) )

import           Data.Text            (Text)
import qualified Data.Text            as T
import Data.Char (ord)
import Data.List (nub)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Char8 (pack, unpack)
-- An efficient compact, immutable byte string type (both strict and lazy)
-- suitable for binary or 8-bit character data.
-- import qualified Data.ByteString.UTF8 as BSUTF (toString, fromString)
-- toString replaces invalid chars with '\xFFFD'
import           Data.Text.Encoding   (decodeUtf8, decodeUtf8', encodeUtf8)
-- decode bytestring to text (exception if not valid)

-- URL encode (escaped)
--import qualified Network.HTTP as HTTP (urlDecode, urlEncode)
import qualified Network.URI          as URI
import qualified Snap.Core            as SN

import Data.Text.ICU.Convert  as ICU  -- all conversion stuff, neede for tests
--import Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as LText
-- import qualified Data.List as L
-- import qualified Data.Text.IO as T (putStrLn)

-- Text (UTF8) -- String
-- trivial, are the same set of values

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

tl2t = LText.toStrict

t2tl = LText.fromStrict

type LazyByteString = Lazy.ByteString

instance Zeros ByteString where zero = t2b ""
instance Zeros LazyByteString where zero = b2bl zero
-- ByteString -- Text
-- bytestring can contain any bitcombinations (binary)

-- bytestring with utf encoded characters
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

-- url encoding - two types url and form data
-- url - in url encode space as %20, as done in the network-uri library (for strings)
--  better name: escape?
-- urlForm - in form as + , as done in the snap core librar (for bytestrings in utf8 endocode)
-- use only for the query part, not the full url!

newtype URL = URL String deriving (Show, Eq)
instance Zeros URL where zero = URL zero
unURL (URL t) = t


s2url :: String -> URL
-- ^ convert string to url   (uses code from Network.HTTP, which converts space into %20)
s2url =   URL . URI.escapeURIString URI.isUnescapedInURIComponent
--s2url =   URL . HTTP.urlEncode

url2s :: URL -> String
-- ^ convert url to string   (uses code from Network.HTTP, which converts space into %20)
url2s  =   URI.unEscapeString . unURL
--url2s a =   HTTP.urlDecode . unURL $ a

testUrlEncodingURI :: String -> Bool
testUrlEncodingURI a = a == (unURL . s2url . url2s . URL $ a)
-- checks if reencoding with HTTP gives the same URL, thus ok encoding
-- input is URL encoded string


url2u = unURL
u2url a = if testUrlEncodingURI a then Just (URL a) else Nothing


s2u :: String -> String
-- ^ convert string to url   (uses code from Network.HTTP, which converts space into %20)
s2u = url2u . s2url

u2s :: String -> Maybe String     --not inverse
-- ^ convert url to string   (uses code from Network.HTTP, which converts space into %20)
u2s  =   fmap url2s . u2url


-- case for encoding of form content (with + for space)

newtype URLform = URLform ByteString deriving (Show, Eq)
unURLform (URLform t) = t


b2urlf :: ByteString -> URLform
-- ^ convert string to url   (uses code from SNAP, which converts space into +)
b2urlf =   URLform . SN.urlEncode

urlf2b :: URLform -> ByteString
-- ^ convert url to string   (uses code from SNAP, which converts space into +)
urlf2b = fromJustNote "urlf2b nothing" . SN.urlDecode . unURLform


testUrlEncodingSNAP :: ByteString -> Bool
testUrlEncodingSNAP a =  maybe False ((a ==). SN.urlEncode) . SN.urlDecode $ a
--testUrlEncodingSNAP a = a == (unURLform . b2urlf . urlf2b . URLform $ a)
-- checks if reencoding with HTTP gives the same URL, thus ok encoding
--testUrlEncodingSNAP a = isJust . SN.urlDecode $ a
-- checks if reencoding with SNAP gives the same URLform, thus ok encoding
-- this test allows control in url encoded strings ...


urlf2u = unURLform
u2urlf a = if testUrlEncodingSNAP a then Just (URLform a) else Nothing
-- this test allows control in url encoded strings ...


b2uf :: ByteString -> ByteString
-- ^ convert ByteString to url   (uses code from SNAP which converts space into +)
b2uf = urlf2u . b2urlf

uf2b :: ByteString -> Maybe ByteString     --not inverse
-- ^ convert url to ByteString   (uses code from SNAP, which converts space into +)
uf2b  =   fmap urlf2b . u2urlf


t2u :: Text -> Text
t2u = s2t . s2u . t2s
u2t :: Text -> Maybe Text
u2t = fmap s2t . u2s . t2s

b2u :: ByteString -> Maybe ByteString
b2u a = (fmap (s2b . s2u) . b2s) $ a
u2b :: ByteString -> Maybe ByteString
u2b = fmap s2b . join  . fmap u2s . b2s


-- | bytestring with latin1 encoded characters
newtype BSlat = BSlat ByteString deriving (Show, Eq)
unBSlat (BSlat a) = a


lat2s :: BSlat -> String
-- ^ bytestring with latin encoding to string
lat2s = latin2s . unBSlat

s2lat :: String -> Maybe BSlat   -- is this always possible ?
-- ^ string encoded as ByteString with latin encoding, if possible
s2lat s =  fmap BSlat . s22latin $ s

s3lat :: String ->  BSlat   -- is this always possible ?
-- ^ string converted to represenatable as latin and then encoded
-- lossy!
s3lat   =  BSlat . s3latin



--prop_s2lat :: String -> Bool  -- will fail ? fails
--prop_s2lat = inverts lat2s s3lat

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
    --    works always, but produces unexpected results if bytestring is not latin encoded
latin2s = Data.ByteString.Char8.unpack
--
s2latin :: String ->  ByteString
        --  works always, but produces unexpected results if bytestring is not latin encoded
s2latin =  Data.ByteString.Char8.pack
----s2latin s = if all  ((<256) . ord) s  then Just . Data.ByteString.Char8.pack else Nothing

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


--prop_s2latin :: String -> Bool     -- why does this always work?  (is the intermediate result ok?)
--prop_s2latin = inverts latin2s s2latin

--prop_s3latin :: String -> Bool     --inverts with reasonable intermediate value
--prop_s3latin s = inverts latin2s s2latin (convertLatin s)

--
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

putIOwords = putStrLn . unlines . map t2s

chars = [198, 216, 197, 206, 219,140,252,202, 419, 420, 1937 ]
difficultBString = ByteString.pack chars
difficultTString = "\198\216\197\206\219\140\252\202\419\420\1937"

-- several char are not latin1
--
--test_latin1 :: IO ()
--test_latin1 = do
--    putIOwords ["latin1 start"]
--    conv <- open "ISO-8859-1" Nothing
--    let lat = difficultBString
----    let uni = "ÆØÅ\DEL\200\SOH\206\219\140\252\202" :: Text
--    let a =   latin2t lat -- toUnicode conv lat :: Text
--    let b =   t2latin a -- fromUnicode conv a
----    let c = fromUnicode conv uni
----    let d = toUnicode conv c :: Text
----    putIOwords [ uni]
--    putIOwords [a,  s2t . show . t2s $ a ]  -- replacement chars for > 256 , but conversion back is ok
--    -- "\198\216\197\206\219\140\252\202\163\164\145"
--    assertEqual b   lat
----    assertEqual   d  uni
--    putIOwords ["latin1 test end" ]
--
--
--test_latin2 :: IO ()  -- roundtrip conversion text - latin - text works (after lossy conversion!)
--test_latin2 = do
--    putIOwords ["latin2 start"]
--    conv <- open "ISO-8859-1" Nothing
--    let a0 = difficultTString
--    putIOwords [" a0 is", a0]
--    putIOwords ["a0 is a difficutl string", s2t $ show a0]
--    let a = s2t . convertLatin . t2s $ a0
----    let uni = "ÆØÅ\DEL\200\SOH\206\219\140\252\202" :: Text
--    let b =   t2latin a -- fromUnicode conv a
--    let c =   latin2t b -- toUnicode conv b
--        -- with conv becomes    "\198\216\197\206\219\140\252\202\SUB\SUB\SUB" and fails
--        -- with latin2t becomes "\198\216\197\206\219\140\252\202\163\164\145" and fails
----    let c = fromUnicode conv uni
----    let d = toUnicode conv c :: Text
----    putIOwords [ uni]
--    putIOwords [" a , c is", a , c, "a==c", s2t $ show (a==c)]
--    putIOwords [" a , c is", s2t $ show a , s2t $  show c]
--    assertEqual a c
----    assertEqual   d  uni
--    putIOwords ["latin2 test end" ]

-- test_fromJust_givesError = assertEqual 1 (fromJustNote' "test_fromJust" (Nothing ::Maybe Int))

-- test_fromJust_old= assertEqual 1 (fromJustNote "test_fromJust" (Nothing ::Maybe Int))


-- -- todo error or strings
-- fromJustNote' msg mb = case mb of
--                             Just r -> r
--                             Nothing -> errorT ["fromJust at ", msg , "with arg", showT mb]
