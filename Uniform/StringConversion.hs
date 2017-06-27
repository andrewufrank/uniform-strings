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
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Uniform.StringConversion (
    ByteString
    , s2b, b2s, b2t,   t2b, t2u,  s2u
    , s2t, t2s
    -- uses UTF8 as encoding in ByteString
    -- urlencode is always represented the same as the input
    , Text (..), BSUTF
    , b2bu, bu2b, bu2s, bu2t, t2bu, s2bu
    , s2bl -- lazy bytestring
    , bb2t, bb2s  -- conversion with error if not UTF8
    , s2latin, t2latin, latin2t, latin2s -- conversion to the latin1 encoding
    , BSlat, s2lat, lat2s, t2lat, lat2t
    , s3lat, t3lat, s3latin, t3latin
    , convertLatin, findNonLatinChars, findNonLatinCharsT
    , htf_thisModulesTests
    , module Safe
    )   where

import           Data.Text.Arbitrary  ()
import           Test.Framework
import           Test.Invariant (inverts)
-- import Data.Maybe
import           Safe
import Control.Monad (join)
--import Data.ByteString.Arbitrary
-- does not produce a simple Arbitrary for Bytestring, could be converted?


-- import "monads-tf" Control.Monad.State (MonadIO, liftIO)

-- import GHC.Exts( IsString(..) )

import           Data.Text            (Text)
import qualified Data.Text            as T
import Data.Char (ord)
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

-- import qualified Data.List as L
-- import qualified Data.Text.IO as T (putStrLn)

-- Text (UTF8) -- String
-- trivial, are the same set of values

s2t :: String -> Text
-- ^ String to Text (invertable)
s2t = T.pack

t2s :: Text -> String
-- ^ String to Text (invertable)
t2s = T.unpack

prop_t2s :: Text -> Bool
prop_t2s = inverts s2t t2s
prop_s2t :: String -> Bool
prop_s2t = inverts t2s s2t


-- ByteString -- Text
-- bytestring can contain any bitcombinations (binary)

-- bytestring with utf encoded characters
newtype BSUTF = BSUTF ByteString deriving (Show, Eq)
unBSUTF (BSUTF a) = a

instance Arbitrary ByteString where arbitrary = ByteString.pack <$> arbitrary
instance Arbitrary BSUTF where
    arbitrary =  do
                    c :: ByteString <- arbitrary
                    let t = testByteStringUtf8 c
                    if t then return (BSUTF c) else arbitrary

t2bu :: Text ->  BSUTF
-- ^ Text to Bytestring (invertable)
t2bu = BSUTF . encodeUtf8

bu2t ::  BSUTF -> Text
-- ^ ByteString to Text --  inverse (not an arbitrary input)
bu2t = decodeUtf8 . unBSUTF

prop_t2bu :: Text -> Bool
prop_t2bu = inverts bu2t t2bu
prop_bu2t :: BSUTF -> Bool
prop_bu2t = inverts t2bu bu2t

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
prop_t2b ::Text -> Bool
prop_t2b a = maybe True (a ==)  (b2t . t2b $ a)

prop_b2t :: ByteString -> Bool
prop_b2t a = maybe True ((a==) . t2b) mb
    where   mb = b2t a
-- bytestring -- string (just a composition of t2s . b2t and reverse)
s2bu :: String ->  BSUTF
-- ^ String to Bytestring (invertable)
s2bu = BSUTF . encodeUtf8 . s2t

--bu2s ::  BSUTF -> String
---- ^ ByteString to String -- not inverse (not any arbitrary input)
--bu2s = t2s . decodeUtf8 . unBSUTF

s2b :: String -> ByteString
s2b = t2b . s2t

s2bl :: String -> Lazy.ByteString
s2bl = Lazy.fromStrict . s2b

b2s :: ByteString -> Maybe String
b2s = fmap t2s . b2t

-- url encoding - two types url and form data
-- url - in url encode space as %20, as done in the network-uri library (for strings)
--  better name: escape?
-- urlForm - in form as + , as done in the snap core librar (for bytestrings in utf8 endocode)

newtype URL = URL String deriving (Show, Eq)
unURL (URL t) = t
instance Arbitrary URL where
    arbitrary = do
            a <- arbitrary
            return . s2url $ a


s2url :: String -> URL
-- ^ convert string to url   (uses code from Network.HTTP, which converts space into %20)
s2url =   URL . URI.escapeURIString URI.isUnescapedInURIComponent
--s2url =   URL . HTTP.urlEncode

url2s :: URL -> String
-- ^ convert url to string   (uses code from Network.HTTP, which converts space into %20)
url2s  =   URI.unEscapeString . unURL
--url2s a =   HTTP.urlDecode . unURL $ a

prop_s2url = inverts url2s s2url
prop_url2s = inverts s2url url2s

testUrlEncodingURI :: String -> Bool
testUrlEncodingURI a = a == (unURL . s2url . url2s . URL $ a)
-- checks if reencoding with HTTP gives the same URL, thus ok encoding
-- input is URL encoded string

test_httpEncode = assertEqual "x%20x" (s2u "x x")
--test_snapEncode = assertEqual "x+x" (b2s . b2u . s2b $  "x x")

url2u = unURL
u2url a = if testUrlEncodingURI a then Just (URL a) else Nothing


s2u :: String -> String
-- ^ convert string to url   (uses code from Network.HTTP, which converts space into %20)
s2u = url2u . s2url

u2s :: String -> Maybe String     --not inverse
-- ^ convert url to string   (uses code from Network.HTTP, which converts space into %20)
u2s  =   fmap url2s . u2url

prop_s2u :: String -> Bool
prop_s2u a = maybe False (a==) (u2s . s2u $ a)

test_s2uA = assertEqual (Just "A") (u2s . s2u $ "A")
test_s2uB = assertEqual (Just " ") (u2s . s2u $ " ")
test_s2uB1 = assertEqual "%20" (s2u " ")
test_s2uC1 = assertEqual False (testUrlEncodingURI "%a ")
--test_s2uC2 = assertEqual ("%20") (HTTP.urlDecode "%a ")
test_s2uC2 = assertEqual "%a " (URI.unEscapeString "%a ")
test_s2uC3 = assertEqual Nothing (u2s "%a ")

prop_u2s :: String -> Bool
prop_u2s a = maybe True ((a==) . s2u) mb
    where   mb = u2s a

-- case for encoding of form content (with + for space)

newtype URLform = URLform ByteString deriving (Show, Eq)
unURLform (URLform t) = t
instance Arbitrary URLform where
    arbitrary = do
            a :: ByteString <- arbitrary
            return . b2urlf $ a


b2urlf :: ByteString -> URLform
-- ^ convert string to url   (uses code from SNAP, which converts space into +)
b2urlf =   URLform . SN.urlEncode

urlf2b :: URLform -> ByteString
-- ^ convert url to string   (uses code from SNAP, which converts space into +)
urlf2b = fromJustNote "urlf2b nothing" . SN.urlDecode . unURLform

prop_b2urlf = inverts urlf2b b2urlf
prop_urlf2b = inverts b2urlf urlf2b

testUrlEncodingSNAP :: ByteString -> Bool
testUrlEncodingSNAP a =  maybe False ((a ==). SN.urlEncode) . SN.urlDecode $ a
--testUrlEncodingSNAP a = a == (unURLform . b2urlf . urlf2b . URLform $ a)
-- checks if reencoding with HTTP gives the same URL, thus ok encoding
--testUrlEncodingSNAP a = isJust . SN.urlDecode $ a
-- checks if reencoding with SNAP gives the same URLform, thus ok encoding
-- this test allows control in url encoded strings ...

test_SNAPEncode = assertEqual "x+x" (b2uf .s2b $ "x x")
--test_snapEncode = assertEqual "x+x" (b2s . b2u . s2b $  "x x")

urlf2u = unURLform
u2urlf a = if testUrlEncodingSNAP a then Just (URLform a) else Nothing
-- this test allows control in url encoded strings ...


b2uf :: ByteString -> ByteString
-- ^ convert ByteString to url   (uses code from SNAP which converts space into +)
b2uf = urlf2u . b2urlf

uf2b :: ByteString -> Maybe ByteString     --not inverse
-- ^ convert url to ByteString   (uses code from SNAP, which converts space into +)
uf2b  =   fmap urlf2b . u2urlf

prop_b2uf :: ByteString -> Bool
prop_b2uf a = maybe False (a==) (uf2b . b2uf $ a)

test_b2uA = assertEqual (Just "A") (uf2b . b2uf . s2b $ "A")
test_b2uB = assertEqual (Just " ") (uf2b . b2uf . s2b $ " ")
test_b2uB1 = assertEqual "+" (b2uf . s2b $ " ")

prop_uf2b :: ByteString -> Bool
prop_uf2b a = maybe True ((a==) . b2uf) mb
    where   mb = uf2b a

test_u2b1 = assertEqual (s2b "%01") (b2uf  "\SOH")
test_u2b2 = assertEqual (s2b "%02") (b2uf "\STX")
test_u2b3 = assertEqual (s2b "%00") (b2uf "\NUL")

test_u2bx1 = assertEqual (Just "\SOH") (uf2b . s2b $ "%01")
test_u2bx2 = assertEqual (Just "\STX")(uf2b . s2b $ "%02")
test_u2bx3 = assertEqual (Just "\NUL") (uf2b . s2b $ "%00")

--test_u2by1 = assertEqual (Just "\SOH") (u2b  "\SOH")
-- with specific test for url:
test_u2by1 = assertEqual Nothing (uf2b  "\SOH")
test_u2sy1 = assertEqual Nothing (u2s  "\SOH")


-- check if snap and network-uri decode the same
-- the difference is only in the encoding

--prop_snap_uri :: String -> Bool
--prop_snap_uri =  (testUrlEncodingSNAP . s2b) <=>  (testUrlEncodingURI )
-- except "!" "$"(see test_u2by2 u2sy2)

--prop_snap_uri_decode :: String -> Bool
--prop_snap_uri_decode =  ( uf2b . s2b) <=>  (fmap s2b . u2s )
-- the actual decoding is different for some chars, e.g.:

test_u2by2 = assertEqual (Just "!") (uf2b  "!")
test_u2sy2 = assertEqual Nothing (u2s  "!")
test_u2by3 = assertEqual (Just "$") (uf2b  "$")
test_u2sy3 = assertEqual Nothing (u2s  "$")
test_u2by4 = assertEqual Nothing (uf2b  "~")
test_u2sy4 = assertEqual (Just "~") (u2s  "~")
test_u2by5 = assertEqual (Just " ")  (uf2b  "+")
test_u2sy5 = assertEqual Nothing(u2s  "+")
test_u2by6 = assertEqual (Just ")")  (uf2b  ")")
test_u2sy6 = assertEqual Nothing(u2s  ")")
test_u2by6a = assertEqual (Just "(")  (uf2b  "(")
test_u2sy6a = assertEqual Nothing(u2s  "(")
test_u2by7 = assertEqual Nothing  (uf2b  "\"")
test_u2sy7 = assertEqual Nothing (u2s  "\"")
test_u2by8 = assertEqual (Just ",")  (uf2b  ",")
test_u2sy8 = assertEqual Nothing (u2s  ",")


t2u :: Text -> Text
t2u = s2t . s2u . t2s
u2t :: Text -> Maybe Text
u2t = fmap s2t . u2s . t2s

b2u :: ByteString -> Maybe ByteString
b2u a = fmap s2b .  fmap s2u . b2s $ a
u2b :: ByteString -> Maybe ByteString
u2b = fmap s2b . join  . fmap u2s . b2s

prop_t2u :: Text -> Bool
prop_t2u a = maybe True (a==) (u2t . t2u $ a)

prop_b2u :: ByteString -> Bool
prop_b2u a = maybe True (a==) (join . fmap u2b . b2u $ a)

prop_u2t :: Text -> Bool
prop_u2t a = maybe True ((a==) . t2u) mb
    where   mb = u2t a

prop_u2b :: ByteString -> Bool
prop_u2b a = maybe True  (a==)  mb
    where   mb = join . fmap b2u . u2b $ a

-- | bytestring with latin1 encoded characters
newtype BSlat = BSlat ByteString deriving (Show, Eq)
unBSlat (BSlat a) = a

instance Arbitrary BSlat where
    arbitrary =  do
                    c :: ByteString <- arbitrary
                    return $ BSlat c
--                    let t = testByteStringUtf8 c
--                    if t then return (BSUTF c) else arbitrary

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


prop_lat2s :: BSlat -> Bool
prop_lat2s = inverts s3lat lat2s

prop_s2lat :: String -> Bool  -- will fail ?
prop_s2lat = inverts lat2s s3lat

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

prop_lat2t :: BSlat -> Bool
prop_lat2t = inverts t3lat lat2t

prop_t3lat :: Text -> Bool   -- fails for \402 \419
prop_t3lat s  = inverts lat2t t3lat (s2t . convertLatin . t2s $ s)


latin2s :: ByteString -> String  --    works always, but produces unexpected results if bytestring is not latin encoded
latin2s = Data.ByteString.Char8.unpack
--
s2latin :: String ->  ByteString  --  works always, but produces unexpected results if bytestring is not latin encoded
s2latin =  Data.ByteString.Char8.pack
--s2latin s = if all  ((<256) . ord) s  then Just . Data.ByteString.Char8.pack else Nothing

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
        '\8222' -> '"'    -- unclear why 8222 but is lower quote
        '\8216' -> '\''    -- unclear why 8218 but is left single quote
        '\8217' -> '\''    -- unclear why 8218 but is right single quote
        '\8218' -> '\''    -- unclear why 8218 but is quote
        '\8221' -> '"'    -- unclear why 8221 but is quote
        '\x2018' -> '\''
        '\x2019' -> '\''

        _ -> '\SUB'    -- could be another char ? \SUB

findNonLatinChars :: String -> String
-- ^ the result is a string of all the characters not in the latin1 encoding
findNonLatinChars = filter (\c -> conv2latinChar c == '\SUB')

findNonLatinCharsT :: Text -> Text
-- ^ the result is a string of all the characters not in the latin1 encoding
findNonLatinCharsT = s2t . filter (\c -> conv2latinChar c /= c) . t2s

prop_latin2s :: ByteString -> Bool
prop_latin2s   = inverts s2latin latin2s -- maybe True ((b ==). latin2s) (s2latin b)

prop_s2latin :: String -> Bool     -- why does this always work?  (is the intermediate result ok?)
prop_s2latin = inverts latin2s s2latin

prop_s3latin :: String -> Bool     --inverts with reasonable intermediate value
prop_s3latin s = inverts latin2s s2latin (convertLatin s)

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
prop_latin2t :: ByteString -> Bool
-- the inermediate text is not always meaningful
prop_latin2t = inverts t2latin latin2t

prop_t2latin :: Text -> Bool
--prop_t2latin t =   inverts latin2t t2latin t   -- fails
prop_t2latin t = if all  ((<256) . ord) (t2s t) then  inverts latin2t t2latin t else True

prop_t22latin :: Text -> Bool
--prop_t2latin t =   inverts latin2t t2latin t   -- fails
prop_t22latin t =  maybe True ((t==) .latin2t) (t22latin t)

putIOwords = putStrLn . unlines . map t2s

chars = [198, 216, 197, 206, 219,140,252,202, 419, 420, 1937 ]
difficultBString = ByteString.pack chars
difficultTString = "\198\216\197\206\219\140\252\202\419\420\1937"

-- several char are not latin1

test_latin1 :: IO ()
test_latin1 = do
    putIOwords ["latin1 start"]
    conv <- open "ISO-8859-1" Nothing
    let lat = difficultBString
--    let uni = "ÆØÅ\DEL\200\SOH\206\219\140\252\202" :: Text
    let a =   latin2t lat -- toUnicode conv lat :: Text
    let b =   t2latin a -- fromUnicode conv a
--    let c = fromUnicode conv uni
--    let d = toUnicode conv c :: Text
--    putIOwords [ uni]
    putIOwords [a,  s2t . show . t2s $ a ]  -- replacement chars for > 256 , but conversion back is ok
    -- "\198\216\197\206\219\140\252\202\163\164\145"
    assertEqual b   lat
--    assertEqual   d  uni
    putIOwords ["latin1 test end" ]


test_latin2 :: IO ()  -- roundtrip conversion text - latin - text works (after lossy conversion!)
test_latin2 = do
    putIOwords ["latin2 start"]
    conv <- open "ISO-8859-1" Nothing
    let a0 = difficultTString
    let a = s2t . convertLatin . t2s $ a0
--    let uni = "ÆØÅ\DEL\200\SOH\206\219\140\252\202" :: Text
    let b =   t2latin a -- fromUnicode conv a
    let c =   latin2t b -- toUnicode conv b   -- with conv becomes  "\198\216\197\206\219\140\252\202\SUB\SUB\SUB" and fails
                        -- with latin2t becomes "\198\216\197\206\219\140\252\202\163\164\145" and fails
--    let c = fromUnicode conv uni
--    let d = toUnicode conv c :: Text
--    putIOwords [ uni]
    putIOwords [a , c]
    assertEqual a c
--    assertEqual   d  uni
    putIOwords ["latin2 test end" ]

-- test_fromJust_givesError = assertEqual 1 (fromJustNote' "test_fromJust" (Nothing ::Maybe Int))

-- test_fromJust_old= assertEqual 1 (fromJustNote "test_fromJust" (Nothing ::Maybe Int))


-- -- todo error or strings
-- fromJustNote' msg mb = case mb of
--                             Just r -> r
--                             Nothing -> errorT ["fromJust at ", msg , "with arg", showT mb]
