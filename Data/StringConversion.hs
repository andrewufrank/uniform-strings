-----------------------------------------------------------------------------
--
-- Module      :  Strings
-- Copyright   :
--
-- | a module with a class for strings, sucht that the normal ops are
--  all polymorphic
-- check which operations are not one-to-one

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

module Data.StringConversion (ByteString
--    , PolyString (..)  -- is this required?
--        ,   headGuarded
            -- these are replacements for Basics
    , s2b, b2s, b2t, t2s, t2b, t2u, s2t, s2u
    -- urlencode is always as Text -- used in web packages
    , Text
    , htf_thisModulesTests
    , conversionTest
    )   where

import Test.Framework
import Test.Invariant
import Data.Text.Arbitrary
--import Data.ByteString.Arbitrary
-- does not produce a simple Arbitrary for Bytestring, could be converted?


import Control.Monad.State (MonadIO, liftIO)

import GHC.Exts( IsString(..) )
import qualified Data.Text as T
import Data.Text (Text)
import Data.ByteString
import qualified Data.ByteString.UTF8 as BSUTF (toString, fromString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8, decodeUtf8')
import qualified Data.Text.IO as T (putStrLn)
import Network.HTTP (urlDecode, urlEncode)
--import Network.CGI.Protocol (maybeRead)
newtype MyString = MyString String deriving (Eq, Show)



s2b :: String ->  ByteString
s2b = BSUTF.fromString -- seems to be s2t . t2b  - with encoding

b2s ::  ByteString -> String   -- not inversable
b2s = BSUTF.toString

s2t :: String -> T.Text
s2t = T.pack

t2s :: T.Text -> String
t2s = T.unpack

t2b :: T.Text ->  ByteString
t2b = encodeUtf8

b2t ::  ByteString -> T.Text  -- not inverse (not any arbitrary input)
b2t = decodeUtf8

--testUrlEncoding :: T.Text -> Bool
testUrlEncoding t = error "i do not see a way to check if proper urlEncoded text"

testByteStringUtf8 :: ByteString -> Bool
-- used for avoiding problems with the quickcheck conversions
testByteStringUtf8 b =
    case decodeUtf8' b of
                -- :: ByteString -> Either UnicodeException Text
                    Left s -> False
                    Right t -> True
t2u :: T.Text -> T.Text  -- url encoded
t2u = s2u . t2s

u2t :: T.Text -> T.Text -- from url encode (not inverse)
u2t = s2t . u2s

s2u :: String -> T.Text
s2u = s2t . urlEncode

u2s :: T.Text -> String     --not inverse
u2s = urlDecode . t2s

--
prop_s2t = inverts s2t t2s
prop_t2s = inverts t2s s2t

prop_u2s = inverts u2s s2u
--prop_s2u = inverts s2u u2s  -- false for "\985" "\54813"
-- u2s -- not all text are proper url encodes fro strings

--instance Arbitrary BSUTF.ByteString

--prop_s2b :: ByteString -> Bool
--prop_s2b = inverts s2b b2s  -- fails ?
-- b2s is total, but replaces codes not in s
prop_b2s = inverts b2s s2b

--prop_t2u = inverts t2u u2t -- fails for text which is not url encoding
prop_u2t = inverts u2t t2u  -- usually passes, fails occasionally, but not yet clear on what input
-- fails for   65535 to 65533 not a character to replacement

--prop_t2b = inverts t2b b2t  -- no aribtrary for bytestring
prop_b2t = inverts b2t t2b

test_b2tx = assertEqual "\65533" (u2t $ t2u "\65535")
-- maps inexactly 65535 to 65533 not a character to replacement

conversionTest = do
    let
        s1 = ""
        a1 = t2u s1
    Prelude.putStrLn . unwords $  (["text a", s1, "url encoded", t2s a1] :: [String] )
    return True

--class PolyString s where
--    pshow :: s -> String
--    punwords :: [s] -> s
--    pputIOwords :: [s] -> ErrIO ()
--
--
--instance PolyString String where
--    pshow = id
--    punwords = unwords
--    pputIOwords = putIOwords
--
--instance PolyString T.Text where
--    pshow = show . T.unpack
--    punwords = T.unwords
--    pputIOwords = putIOwords . map t2s
--
--instance PolyString BSUTF.ByteString where
--    pshow =   show . BSUTF.toString
----instance (IsString m) => PolyString m where
----    pshow = show  . fromString
