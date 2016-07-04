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
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
--    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
--    , FlexibleContexts
--    , DeriveFunctor
--    , ScopedTypeVariables
--    , OverlappingInstances
    , UndecidableInstances
    , OverloadedStrings
--    , TypeFamilies
    , NoMonomorphismRestriction
    #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.StringConversion (
    ByteString
--    , PolyString (..)  -- is this required?
--        ,   headGuarded
            -- these are replacements for Basics
    , s2b, b2s, b2t, t2s, t2b, t2u, s2t, s2u
    -- uses UTF8 as encoding in ByteString
    -- urlencode is always represented the same as the input
    , Text
    , htf_thisModulesTests
    , conversionTest
    )   where

import Test.Framework
import Test.Invariant
import Data.Text.Arbitrary
import Data.Maybe

--import Data.ByteString.Arbitrary
-- does not produce a simple Arbitrary for Bytestring, could be converted?


import Control.Monad.State (MonadIO, liftIO)

import GHC.Exts( IsString(..) )
import qualified Data.Text as T
import qualified Data.List as L
import Data.Text (Text)
import Data.ByteString
import qualified Data.ByteString.UTF8 as BSUTF (toString, fromString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8, decodeUtf8')
import qualified Data.Text.IO as T (putStrLn)
import Network.HTTP (urlDecode, urlEncode)
import qualified Snap.Core as SN
--import Network.CGI.Protocol (maybeRead)
newtype MyString = MyString String deriving (Eq, Show)



s2b :: String ->  ByteString
-- ^ String to ByteString conversion (invertable)
s2b = BSUTF.fromString -- seems to be s2t . t2b  - with encoding

b2s ::  ByteString -> String
-- ^ ByteString to String - not inversable (replacement of non-representable chars)
b2s = BSUTF.toString

s2t :: String -> T.Text
-- ^ String to Text (invertable)
s2t = T.pack

t2s :: T.Text -> String
-- ^ String to Text (invertable)
t2s = T.unpack

t2b :: T.Text ->  ByteString
-- ^ Text to Bytestring (invertable)
t2b = encodeUtf8

b2t ::  ByteString -> T.Text
-- ^ ByteString to Text -- not inverse (not any arbitrary input)
b2t = decodeUtf8

t2u :: T.Text -> T.Text  -- url encoded
-- ^ convert text to url (uses code from Network.HTTP, which converts space into %20)
t2u = s2t . s2u . t2s

u2t :: T.Text -> T.Text -- from url encode (not inverse)
-- ^ convert url to text (uses code from Network.HTTP, which converts space into %20)
u2t = s2t . u2s . t2s

s2u :: String -> String
-- ^ convert string to url   (uses code from Network.HTTP, which converts space into %20)
s2u =   urlEncode

u2s :: String -> String     --not inverse
-- ^ convert url to string   (uses code from Network.HTTP, which converts space into %20)
u2s = urlDecode

u2b :: ByteString -> Maybe ByteString
-- ^ convert url to bytestring   (uses code from snap.core, which converts space into +)
u2b = SN.urlDecode

b2u :: ByteString ->  ByteString
-- ^ convert url to bytestring in url (uses code from snap.core, which converts space into +)
b2u = SN.urlEncode

prop_t2s = inverts s2t t2s
prop_s2t = inverts t2s s2t

prop_s2u = inverts u2s s2u
--prop_s2u = inverts s2u u2s  -- false for "\985" "\54813"
-- u2s -- not all text are proper url encodes fro strings

--instance Arbitrary BSUTF.ByteString

--prop_s2b :: ByteString -> Bool
--prop_s2b = inverts s2b b2s  -- fails ?
-- b2s is total, but replaces codes not in s
prop_s2b = inverts b2s s2b

--prop_t2u = inverts t2u u2t -- fails for text which is not url encoding
prop_u2ta a = if "\65535" `T.isInfixOf` a then True
            else inverts u2t t2u a -- usually passes, fails occasionally, but not yet clear on what input
-- fails for   65535 to 65533 not a character to replacement
prop_u2tb a = if testUrlEncoding a then inverts u2t t2u a
                    else True
--prop_t2u a = if testUrlEncoding a then inverts t2u u2t a else True
-- is not inverse because result is encoded, input not


--prop_t2b = inverts t2b b2t  -- no aribtrary for bytestring
prop_t2b = inverts b2t t2b

--test_b2tx = assertEqual "\65533" (u2t $ t2u "\65535")
---- maps inexactly 65535 to 65533 not a character to replacement
--test_t2u1 = assertEqual "\SYN{" (u2t "\SYN{")
--test_t2u2  = assertEqual "\22155\63245" (u2t "\22155\63245")
--
--test_t2u3 = assertEqual "%16%7B" (t2u "\SYN{")
--test_t2u4  = assertEqual "%E5%9A%8B%EF%9C%8D" (t2u "\22155\63245")

--prop_b2u a =   maybe True ((a==). b2t .  b2u) . u2b $ (t2b a)
-- some inputs cannot convert, how to filter?
prop_u2b a =   maybe True ((a==). b2t) .  u2b  . b2u $ (t2b a)
--prop_u2b = inverts u2b b2u

--prop_urlEncode :: String -> Bool
--prop_urlEncode a = if " " `L.isInfixOf` a then True
--                    else
--                        (s2b . s2u $ a) == (b2u . s2b $ a)

-- compare the urlencode of HTTP and snap

test_httpEncode = assertEqual "x%20x" (s2u "x x")
test_snapEncode = assertEqual "x+x" (b2s . b2u . s2b $  "x x")

testUrlEncoding :: T.Text -> Bool
testUrlEncoding t = isJust . SN.urlDecode . t2b $ t
-- ^ test wheter a given text does not contain characters not
-- permitted when url encoding  using
--urlDecode :: ByteString -> Maybe ByteString


testByteStringUtf8 :: ByteString -> Bool
-- ^ test whether a byte string is valid utf8 encoded
-- used for avoiding problems with the quickcheck conversions
testByteStringUtf8 b =
    case decodeUtf8' b of
                -- :: ByteString -> Either UnicodeException Text
                    Left s -> False
                    Right t -> True

-- | url encoding of space is not always handled the same way (+ or %20)
conversionTest = do
    let
        s1 = " " :: String
        a1 = s2u s1 :: String
    Prelude.putStrLn . unwords $  (["string space:", s1, "gives url encoded",  a1] :: [String] )

    let b2 = s2b s1
        a2  = b2u b2
    Prelude.putStrLn . unwords $  (["bytestring space:", show b2
                , "gives url encoded", show  $ a2] :: [String] )
    return  (s1 == u2s a1 || (maybe False (b2 ==) $  u2b a2))


