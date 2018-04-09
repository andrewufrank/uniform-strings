 -----------------------------------------------------------------------------
--
-- Module      :  Strings
-- Copyright   :
--
-- | a module with a class for strings, sucht that the normal ops are
--  all polymorphic for string and text (and total)
-- the string (i.e. [Char]) functions are the semantic definitions,
-- the other implementation are tested against these.
-- except intercalate, which returns Maybe
-- (the corresponding restrictions for the unlines and unwords functions are not enforced)
--
-- performance can be improved by using the "native" functions
-- could be expanded

-- class niceStrings can be replaced or integrated in the generic strings
-- it may be useful to have more than one show like operation
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Strings.Utilities_test
    where

import Uniform.Zero
import Uniform.ListForm
import  Uniform.Strings.Utilities
import  Uniform.Strings.Conversion

--import           Algebra.Laws             as Law
import           Test.Framework
import Data.Text.Arbitrary
import           Test.Invariant           as Rule

-- probably better just to move these module to package uniform-algebra
-- but there is so far only zero

import           Data.Char                (isSpace, isLower, toLower, toUpper)
import           Text.Printf              (PrintfArg, PrintfType, printf)

import           Data.List                as L
--import qualified Data.Vector              as V
import           GHC.Exts                 (IsString (..))

--import qualified Data.List.Split          as S
import           Data.Maybe
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
--import           Safe
-- import           Uniform.Error            (fromJustNote)
-- not possible, because Error is based on String
--import           Uniform.StringConversion
import qualified Data.ByteString.Lazy as Lazy


prop_zero_mknull_string :: String -> Bool
prop_zero_mknull_string = prop_zero_mknull

prop_assoz_string :: String -> String -> String -> Bool
prop_assoz_string  = prop_assoz

prop_concat_string :: [String] -> Bool
prop_concat_string  =  prop_concat

prop_filterChar_string :: String -> Bool
prop_filterChar_string a = all (cond)    af
      where
          cond x = x `notElem` ['a', '\r', '1']
          af = (filterChar cond a)


--
prop_zero_mknull_text :: Text -> Bool
prop_zero_mknull_text = prop_zero_mknull

prop_assoz_text :: Text -> Text -> Text -> Bool
prop_assoz_text  = prop_assoz

prop_concat_text :: [Text] -> Bool
prop_concat_text  =  prop_concat

prop_filterChar_text :: Text -> Bool
prop_filterChar_text = prop_filterChar



-- tests that text operations have same semantics than string2maybe    putIOwords  =liftIOstrings
prop_unwords :: [String]  -> Bool
prop_unwords =     unwords' . map s2t  <=>  s2t . unwords

prop_words =  words' . s2t <=> map s2t . words

-- not invertible for [""] and not when strings include " "
-- same problem as intervalate!
-- same problem for lines and unlines
--prop_wordsInverse :: [String] -> Bool
--prop_wordsInverse = inverts words' unwords'

prop_unlines = unlines' . map s2t <=>  s2t . unlines
prop_lines = lines' . s2t <=> map s2t . lines

prop_append :: String -> String -> Bool
prop_append a b = append' (s2t a) (s2t b) == s2t  (append a  b)

prop_append2 :: String -> String -> Bool
prop_append2 a b = append a b == reverse (append (reverse b) (reverse a))

prop_null = null' . s2t <=> null
-- prop_toUpper = toUpper' . s2t <=> s2t . toUpper'  -- failed for "\223" sz ligature
prop_toLower = toLower' . s2t <=> s2t . toLower'

--prop_toLowerInvers :: String -> Bool
--prop_toLowerInvers = inverts toLower' toUpper'

prop_toLower1 :: String -> Bool
prop_toLower1 = idempotent toLower'
prop_toLower2 :: Text -> Bool
prop_toLower2 = idempotent toLower'

prop_toUpper1 :: String -> Bool
prop_toUpper1 = idempotent toUpper'
prop_toUpper2 :: Text -> Bool
prop_toUpper2 = idempotent toUpper'

-- conversin of lowercase to uper and back is not inverse
--prop_inverse_toUpper :: String -> Bool
--prop_inverse_toUpper a = inverts toLower' toUpper' (toLower' a)
--
--prop_inverse_toUpperT :: Text -> Bool
--prop_inverse_toUpperT a = inverts toLower' toUpper' (toLower' a)

--prop_inverse_toLower :: String -> Bool
--prop_inverse_toLower a = inverts toUpper' toLower' (toUpper' a)
--
--prop_inverse_toLowerT :: Text -> Bool
--prop_inverse_toLowerT a = inverts toUpper' toLower' (toUpper' a)

prop_isPrefixOf a b = isPrefixOf' (s2t a) (s2t b) == isPrefixOf' a b
prop_isInfixOf' a b = isInfixOf' (s2t a) (s2t b) == isInfixOf' a b
prop_stripPrefix'f a b = stripPrefix' (s2t a) (s2t b) == fmap s2t  (stripPrefix' a b)

prop_stripPrefix2 :: String -> String -> Bool
prop_stripPrefix2 a b = stripPrefix' a (append' a b) == Just b

-- establish inverse for strings
prop_inverse_intercalate :: String -> [String] -> Bool
prop_inverse_intercalate s a = maybe True (isJust . fmap (a==) . splitOn' s) ( intercalate' s a )
--prop_intercalate :: String -> [String] -> Bool
--prop_intercalate a b = intercalate' (s2t a) (map s2t b) ==  fmap s2t (intercalate' a b)

prop_trim = trim' . s2t  <=> s2t . trim'
prop_trim2 :: String -> Bool
prop_trim2 = idempotent trim'
prop_trim3 :: Text -> Bool
prop_trim3 = idempotent trim'

prop_splitOn_text a b = splitOn' (s2t a) (s2t b)  == fmap (map s2t) (splitOn' a b)
prop_splitOn_bytestring a b =
        splitOn' (s2bu a) (s2bu b)  == fmap (map s2bu) (splitOn' a b)

prop_splitOn_intercalate :: String -> [String] -> Bool
prop_splitOn_intercalate a b =
    if null a then True
        else if any (a `isInfixOf'`) b then True
                else    (maybe True  (b==))  (maybe Nothing (splitOn' a)
                            ( intercalate' a $ b))
        -- fails on "" [""]

test_splitOn = assertBool ( Just [] == splitOn' [] ("a"::String))
test_bu2s = assertEqual ("a"::String) (toString . t2bu $ ("a" :: Text))

--test_show'_forText = assertEqual ("a"::String) (show' ("a"::String))
-- gives overlapping with [a]
test_show'_forString = assertEqual ("a"::Text) (show' ("a"::Text))

--stringTest :: IO Bool
--stringTest = do
--    let
--        r1 = splitOn'  (""::String) ("a"::String)   :: Maybe [String]
--        r2 = splitOn'  (""::Text) ("a"::Text)  :: Maybe [Text]
--        v1 = S.splitOn  (""::String) ("a"::String)   ::  [String]
----        v2 = T.splitOn   (""::Text) ("a"::Text)  ::  [Text]  -- produces error
--        w1 = S.splitOn  ("x"::String) ("a"::String)   ::  [String]
--        w2 = T.splitOn   ("x"::Text) ("a"::Text)  ::  [Text]
--        c1 = L.intercalate  ("a"::String) ([]::[String])   :: String
--        c2 = T.intercalate  ("a"::Text) ([]::[Text])   ::Text
--        d1 = intercalate' ("a"::String) ([]::[String])   ::Maybe String
--        d2 = intercalate' ("a"::Text) ([]::[Text])   ::Maybe Text
--    putIOwordsS ["splitOn on empty input \nfor string", show r1]
--    putIOwords ["\nfor text", show  r2, "--"]
--    putIOwordsS ["S.splitOn on empty input \nfor string", show v1] --  ["","a"]
----    putIOwords ["\nT. for text", show  v2, "--"] -- produces error
--    putIOwordsS ["S.splitOn on x input \nfor string", show w1] --  ["","a"]
--    putIOwords ["\nT. for text", show  w2, "--"] -- produces error
--    putIOwords ["intercalate for string",  c1, "-"]
--    putIOwords ["intercalate for text",   t2s c2, "-"]
--    putIOwords ["intercalate' for string", show  c1, "-"]
--    putIOwords ["intercalate' for text",   show  c2, "-"]
--    return True
