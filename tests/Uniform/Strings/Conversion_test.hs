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
--{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Strings.Conversion_test  where

import           Data.Text.Arbitrary  ()
import           Test.Framework
import           Test.Invariant (inverts)
import Uniform.Strings.Conversion
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Char8 (pack, unpack)
import qualified Network.URI          as URI
import qualified Snap.Core            as SN

--import           Safe
--import Control.Monad (join)
--import Data.ByteString.Arbitrary
-- does not produce a simple Arbitrary for Bytestring, could be converted?


-- import "monads-tf" Control.Monad.State (MonadIO, liftIO)

-- import GHC.Exts( IsString(..) )

--import           Data.Text            (Text)
--import qualified Data.Text            as T
import Data.Char (ord)
import Control.Monad (join)
import           Data.Text.Encoding   (decodeUtf8, decodeUtf8', encodeUtf8)
--import           Data.ByteString      (ByteString)
--import qualified Data.ByteString      as ByteString
--import qualified Data.ByteString.Lazy as Lazy
--import Data.ByteString.Char8 (pack, unpack)
---- An efficient compact, immutable byte string type (both strict and lazy)
---- suitable for binary or 8-bit character data.
---- import qualified Data.ByteString.UTF8 as BSUTF (toString, fromString)
---- toString replaces invalid chars with '\xFFFD'
--import           Data.Text.Encoding   (decodeUtf8, decodeUtf8', encodeUtf8)
---- decode bytestring to text (exception if not valid)
--
---- URL encode (escaped)
----import qualified Network.HTTP as HTTP (urlDecode, urlEncode)
--import qualified Network.URI          as URI
--import qualified Snap.Core            as SN
--
--import Data.Text.ICU.Convert  as ICU  -- all conversion stuff, neede for tests

instance Arbitrary ByteString where arbitrary = ByteString.pack <$> arbitrary
instance Arbitrary BSUTF where
    arbitrary =  do
                    c :: ByteString <- arbitrary
                    let t = testByteStringUtf8 c
                    if t then return (BSUTF c) else arbitrary
instance Arbitrary URL where
    arbitrary = do
            a <- arbitrary
            return . s2url $ a
instance Arbitrary URLform where
    arbitrary = do
            a :: ByteString <- arbitrary
            return . b2urlf $ a
instance Arbitrary BSlat where
    arbitrary =  do
                    c :: ByteString <- arbitrary
                    return $ BSlat c
--                    let t = testByteStringUtf8 c
--                    if t then return (BSUTF c) else arbitrary

prop_t2b ::Text -> Bool
prop_t2b a = maybe True (a ==)  (b2t . t2b $ a)

prop_b2t :: ByteString -> Bool
prop_b2t a = maybe True ((a==) . t2b) mb
    where   mb = b2t a

prop_t2bu :: Text -> Bool
prop_t2bu = inverts bu2t t2bu
prop_bu2t :: BSUTF -> Bool
prop_bu2t = inverts t2bu bu2t

test_SNAPEncode = assertEqual "x+x" (b2uf .s2b $ "x x")
--test_snapEncode = assertEqual "x+x" (b2s . b2u . s2b $  "x x")

prop_t2s :: Text -> Bool
prop_t2s = inverts s2t t2s
prop_s2t :: String -> Bool
prop_s2t = inverts t2s s2t


testByteStringUtf8 :: ByteString -> Bool
-- ^ test whether a byte string is valid utf8 encoded
-- used for avoiding problems with the quickcheck conversions
testByteStringUtf8 b =
    case decodeUtf8' b of
                -- :: ByteString -> Either UnicodeException Text
                    Left s  -> False
                    Right t -> True


prop_s2url = inverts url2s s2url
prop_url2s = inverts s2url url2s


test_httpEncode = assertEqual "x%20x" (s2u "x x")
--test_snapEncode = assertEqual "x+x" (b2s . b2u . s2b $  "x x")



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

--newtype URLform = URLform ByteString deriving (Show, Eq)
--unURLform (URLform t) = t
--instance Arbitrary URLform where
--    arbitrary = do
--            a :: ByteString <- arbitrary
--            return . b2urlf $ a

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


prop_b2urlf = inverts urlf2b b2urlf
prop_urlf2b = inverts b2urlf urlf2b

testUrlEncodingSNAP :: ByteString -> Bool
testUrlEncodingSNAP a =  maybe False ((a ==). SN.urlEncode) . SN.urlDecode $ a
--testUrlEncodingSNAP a = a == (unURLform . b2urlf . urlf2b . URLform $ a)
-- checks if reencoding with HTTP gives the same URL, thus ok encoding
--testUrlEncodingSNAP a = isJust . SN.urlDecode $ a
-- checks if reencoding with SNAP gives the same URLform, thus ok encoding
-- this test allows control in url encoded strings ...
--
--test_SNAPEncode = assertEqual "x+x" (b2uf .s2b $ "x x")
----test_snapEncode = assertEqual "x+x" (b2s . b2u . s2b $  "x x")
--
--
--
--prop_b2uf :: ByteString -> Bool
--prop_b2uf a = maybe False (a==) (uf2b . b2uf $ a)
--
--test_b2uA = assertEqual (Just "A") (uf2b . b2uf . s2b $ "A")
--test_b2uB = assertEqual (Just " ") (uf2b . b2uf . s2b $ " ")
--test_b2uB1 = assertEqual "+" (b2uf . s2b $ " ")
--
--prop_uf2b :: ByteString -> Bool
--prop_uf2b a = maybe True ((a==) . b2uf) mb
--    where   mb = uf2b a
--
--test_u2b1 = assertEqual (s2b "%01") (b2uf  "\SOH")
--test_u2b2 = assertEqual (s2b "%02") (b2uf "\STX")
--test_u2b3 = assertEqual (s2b "%00") (b2uf "\NUL")
--
--test_u2bx1 = assertEqual (Just "\SOH") (uf2b . s2b $ "%01")
--test_u2bx2 = assertEqual (Just "\STX")(uf2b . s2b $ "%02")
--test_u2bx3 = assertEqual (Just "\NUL") (uf2b . s2b $ "%00")
--
----test_u2by1 = assertEqual (Just "\SOH") (u2b  "\SOH")
---- with specific test for url:
--test_u2by1 = assertEqual Nothing (uf2b  "\SOH")
--test_u2sy1 = assertEqual Nothing (u2s  "\SOH")
--
--
---- check if snap and network-uri decode the same
---- the difference is only in the encoding
--
----prop_snap_uri :: String -> Bool
----prop_snap_uri =  (testUrlEncodingSNAP . s2b) <=>  (testUrlEncodingURI )
---- except "!" "$"(see test_u2by2 u2sy2)
--
----prop_snap_uri_decode :: String -> Bool
----prop_snap_uri_decode =  ( uf2b . s2b) <=>  (fmap s2b . u2s )
---- the actual decoding is different for some chars, e.g.:
--
--test_u2by2 = assertEqual (Just "!") (uf2b  "!")
--test_u2sy2 = assertEqual Nothing (u2s  "!")
--test_u2by3 = assertEqual (Just "$") (uf2b  "$")
--test_u2sy3 = assertEqual Nothing (u2s  "$")
--test_u2by4 = assertEqual Nothing (uf2b  "~")
--test_u2sy4 = assertEqual (Just "~") (u2s  "~")
--test_u2by5 = assertEqual (Just " ")  (uf2b  "+")
--test_u2sy5 = assertEqual Nothing(u2s  "+")
--test_u2by6 = assertEqual (Just ")")  (uf2b  ")")
--test_u2sy6 = assertEqual Nothing(u2s  ")")
--test_u2by6a = assertEqual (Just "(")  (uf2b  "(")
--test_u2sy6a = assertEqual Nothing(u2s  "(")
--test_u2by7 = assertEqual Nothing  (uf2b  "\"")
--test_u2sy7 = assertEqual Nothing (u2s  "\"")
--test_u2by8 = assertEqual (Just ",")  (uf2b  ",")
--test_u2sy8 = assertEqual Nothing (u2s  ",")
--
--
--prop_b2uf :: ByteString -> Bool
--prop_b2uf a = maybe False (a==) (uf2b . b2uf $ a)
--
--test_b2uA = assertEqual (Just "A") (uf2b . b2uf . s2b $ "A")
--test_b2uB = assertEqual (Just " ") (uf2b . b2uf . s2b $ " ")
--test_b2uB1 = assertEqual "+" (b2uf . s2b $ " ")
--
--prop_uf2b :: ByteString -> Bool
--prop_uf2b a = maybe True ((a==) . b2uf) mb
--    where   mb = uf2b a
--
--test_u2b1 = assertEqual (s2b "%01") (b2uf  "\SOH")
--test_u2b2 = assertEqual (s2b "%02") (b2uf "\STX")
--test_u2b3 = assertEqual (s2b "%00") (b2uf "\NUL")
--
--test_u2bx1 = assertEqual (Just "\SOH") (uf2b . s2b $ "%01")
--test_u2bx2 = assertEqual (Just "\STX")(uf2b . s2b $ "%02")
--test_u2bx3 = assertEqual (Just "\NUL") (uf2b . s2b $ "%00")
--
----test_u2by1 = assertEqual (Just "\SOH") (u2b  "\SOH")
---- with specific test for url:
--test_u2by1 = assertEqual Nothing (uf2b  "\SOH")
--test_u2sy1 = assertEqual Nothing (u2s  "\SOH")
--
--prop_s2u :: String -> Bool
--prop_s2u a = maybe False (a==) (u2s . s2u $ a)
--
--test_s2uA = assertEqual (Just "A") (u2s . s2u $ "A")
--test_s2uB = assertEqual (Just " ") (u2s . s2u $ " ")
--test_s2uB1 = assertEqual "%20" (s2u " ")
--test_s2uC1 = assertEqual False (testUrlEncodingURI "%a ")
----test_s2uC2 = assertEqual ("%20") (HTTP.urlDecode "%a ")
--test_s2uC2 = assertEqual "%a " (URI.unEscapeString "%a ")
--test_s2uC3 = assertEqual Nothing (u2s "%a ")
--
--prop_u2s :: String -> Bool
--prop_u2s a = maybe True ((a==) . s2u) mb
--    where   mb = u2s a
--prop_b2urlf = inverts urlf2b b2urlf
--prop_urlf2b = inverts b2urlf urlf2b
--
--prop_s2url = inverts url2s s2url
--prop_url2s = inverts s2url url2s

testUrlEncodingURI :: String -> Bool
testUrlEncodingURI a = a == (unURL . s2url . url2s . URL $ a)
-- checks if reencoding with HTTP gives the same URL, thus ok encoding
-- input is URL encoded string

--test_httpEncode = assertEqual "x%20x" (s2u "x x")
--test_snapEncode = assertEqual "x+x" (b2s . b2u . s2b $  "x x")

-- check if snap and network-uri decode the same
-- the difference is only in the encoding

--prop_snap_uri :: String -> Bool
--prop_snap_uri =  (testUrlEncodingSNAP . s2b) <=>  (testUrlEncodingURI )
-- except "!" "$"(see test_u2by2 u2sy2)

--prop_snap_uri_decode :: String -> Bool
--prop_snap_uri_decode =  ( uf2b . s2b) <=>  (fmap s2b . u2s )
-- the actual decoding is different for some chars, e.g.:

--test_u2by2 = assertEqual (Just "!") (uf2b  "!")
--test_u2sy2 = assertEqual Nothing (u2s  "!")
--test_u2by3 = assertEqual (Just "$") (uf2b  "$")
--test_u2sy3 = assertEqual Nothing (u2s  "$")
--test_u2by4 = assertEqual Nothing (uf2b  "~")
--test_u2sy4 = assertEqual (Just "~") (u2s  "~")
--test_u2by5 = assertEqual (Just " ")  (uf2b  "+")
--test_u2sy5 = assertEqual Nothing(u2s  "+")
--test_u2by6 = assertEqual (Just ")")  (uf2b  ")")
--test_u2sy6 = assertEqual Nothing(u2s  ")")
--test_u2by6a = assertEqual (Just "(")  (uf2b  "(")
--test_u2sy6a = assertEqual Nothing(u2s  "(")
--test_u2by7 = assertEqual Nothing  (uf2b  "\"")
--test_u2sy7 = assertEqual Nothing (u2s  "\"")
--test_u2by8 = assertEqual (Just ",")  (uf2b  ",")
--test_u2sy8 = assertEqual Nothing (u2s  ",")



--


-- | bytestring with latin1 encoded characters
--newtype BSlat = BSlat ByteString deriving (Show, Eq)
--unBSlat (BSlat a) = a

--instance Arbitrary BSlat where
--    arbitrary =  do
--                    c :: ByteString <- arbitrary
--                    return $ BSlat c
----                    let t = testByteStringUtf8 c
----                    if t then return (BSUTF c) else arbitrary
--


prop_lat2s :: BSlat -> Bool
prop_lat2s = inverts s3lat lat2s

--prop_s2lat :: String -> Bool  -- will fail ? fails
--prop_s2lat = inverts lat2s s3lat


prop_lat2t :: BSlat -> Bool
prop_lat2t = inverts t3lat lat2t

--prop_t3lat :: Text -> Bool   -- fails for \402 \419
--prop_t3lat s  = inverts lat2t t3lat (s2t . convertLatin . t2s $ s)


prop_latin2s :: ByteString -> Bool
prop_latin2s   = inverts s2latin latin2s -- maybe True ((b ==). latin2s) (s2latin b)

--prop_s2latin :: String -> Bool     -- why does this always work?  (is the intermediate result ok?)
--prop_s2latin = inverts latin2s s2latin

--prop_s3latin :: String -> Bool     --inverts with reasonable intermediate value
--prop_s3latin s = inverts latin2s s2latin (convertLatin s)


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
