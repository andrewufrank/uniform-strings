-----------------------------------------------------------------------------
--
-- Module      :  Strings
-- Copyright   :
--
-- | a module with a class for strings, sucht that the normal ops are
--  all polymorphic for string and text (and total)
--
-- could be expanded
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
    , FlexibleInstances
    , FlexibleContexts
    , UndecidableInstances
    , OverloadedStrings
    , NoMonomorphismRestriction
    #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.StringUtilities
    (
    Strings (..), putIOwordsT, putIOwordsS
--    , Strings2 (..)
    , unlinesT
    , sortCaseInsensitive, cmpCaseInsensitive
    , maybe2string
    , htf_thisModulesTests
    , stringTest
    )
    where

import Test.Framework
import Test.Invariant

import Data.Char (toUpper, toLower, isSpace)
import Control.Monad.State (MonadIO, liftIO)

import GHC.Exts( IsString(..) )
import qualified Data.Text as T
import Data.List as L
import Data.StringConversion
import Data.Maybe
import qualified Data.List.Split as S
import Safe
--
-- | generalized functions to work on strings, text and bytestring
-- with the same semantics
class Strings a where

    toString ::  a -> String
    toText :: Show a => a -> Text
    -- ^ conversion

    putIOwords :: MonadIO m =>  [a] -> m ()
    -- convenience function for simple output formating

    unwords' :: [a] -> a
    words' :: a -> [a]
    unlines' :: [a] -> a
    lines' :: a -> [a]
--    punwords :: [a] -> s
    toText = s2t . show
    append', append  :: a -> a -> a
    append = append' -- without ' to maintain old code
    null' :: a -> Bool
    toLower' :: a -> a
    -- ^ convert the string to  lowercase, idempotent
    -- is not inverse of toUpper
    toUpper':: a -> a
    -- is not idempotent and gives different results for string and text (sz and similar ligatures)

    isPrefixOf', isInfixOf' :: a -> a -> Bool
    stripPrefix' :: a -> a -> Maybe a
    concat' :: [a] -> a
    trim' :: a -> a
    -- ^ removes all spaces front and back, idempotent
    intercalate' :: a -> [a] -> Maybe a
    -- ^ splitOn' and intercalate' are inverses (see Data.SplitList)
    -- returns Nothing if second  is empty and intercalate "x" "" gives Just ""
    -- return Nothing if first is empty or contained in second to achievee inverse with splitOn
    splitOn' :: a -> a -> Maybe [a]
    -- ^ returns Nothing if second is empty

--class Strings2 x a where
--    show' ::  x -> a
-- replaced with toString or toText

liftIOstrings ::MonadIO m => [String] -> m ()
liftIOstrings   = liftIO . putStrLn. unwords


instance Strings String where
    toString = id
    toText = s2t

    putIOwords  =liftIOstrings
    unwords' = unwords
    words' = words
    unlines' = unlines
    lines' = lines
    append' = (++)
    null' = null
    toUpper' = map toUpper
    toLower' = map toLower
    concat' = concat
    isPrefixOf'  = isPrefixOf
    isInfixOf' = isInfixOf
    stripPrefix'  = stripPrefix
    intercalate' s a = if null a then Nothing else
                            if null s then Nothing
                                else if (s `isInfixOf` (concat' a)) then Nothing
                                    else Just $ L.intercalate s a
    splitOn' o s= if null' o then Just []
                        else if null' s then (Just [""]) else Just $ S.splitOn o s
    trim' = f . f
        where f = reverse . dropWhile isSpace

--instance Strings2 String String where
--    show' =  id
--instance (Show x ) => Strings2 x String where
--    show' = show

instance Strings T.Text where
    toString = t2s
    toText = id

    putIOwords = liftIOstrings . map T.unpack
    unwords' = T.unwords
    words' =  T.words
    lines' = T.lines
    unlines' = T.unlines
    append' = T.append
    null' = T.null
    toUpper' = T.toUpper
    toLower' = T.toLower
    concat' = T.concat
    isPrefixOf' = T.isPrefixOf
    isInfixOf' = T.isInfixOf
    stripPrefix' = T.stripPrefix
    intercalate' s a = if null a then (Nothing :: Maybe Text) else
                            if null' s then Nothing
                                else if (s `isInfixOf'` (concat' a)) then Nothing
                                    else Just $ T.intercalate s a
    splitOn' o s= if null' o then Just []
                        else if null' s then (Just [""]) else Just $ T.splitOn o s
    trim' = s2t . trim' . t2s
--    splitOn' o s= if null' o then Just []
--                        else if null' s then Nothing else Just $ T.splitOn o s

instance Strings ByteString  where
-- doubtful - what is possible without error? t2b . b2t is not id
--  all achieved by transforming the input to text and operating on this.
-- result cannot be translated back
-- alternatively used Data.ByteString.Char8 -- which assumes that only 8 bit char

    toString = b2s
    toText = b2t

    putIOwords = liftIOstrings . map b2s
    unwords' = t2b . unwords' . map b2t
    words' =  map t2b .  words' . b2t
    lines' = map t2b . lines' . b2t
    unlines' =  t2b . unlines' . map b2t

    append' a b = t2b . append' (b2t a) $ b2t b
    null' = T.null . b2t
    toUpper' = t2b . toUpper' . b2t
    toLower' = t2b . toLower' . b2t
    concat' = t2b . concat' . map b2t
    isPrefixOf' t s  = isPrefixOf' (b2t s) (b2t t)
    isInfixOf' t s  = isInfixOf' (b2t s) (b2t t)
    stripPrefix' p s = fmap t2b $  stripPrefix' (b2t p)  (b2t s)
    intercalate' x a  =  fmap t2b  (intercalate' (b2t x) (map b2t a))
    splitOn' o s = fmap (fmap t2b) $ splitOn' (b2t o) (b2t s)
    trim' = s2b . trim' . b2s


instance Strings Int where
    toString = show

instance Strings Bool where
    toString = show

stringTest :: IO Bool
stringTest = do
    let
        r1 = splitOn'  (""::String) ("a"::String)   :: Maybe [String]
        r2 = splitOn'  (""::Text) ("a"::Text)  :: Maybe [Text]
        v1 = S.splitOn  (""::String) ("a"::String)   ::  [String]
--        v2 = T.splitOn   (""::Text) ("a"::Text)  ::  [Text]  -- produces error
        w1 = S.splitOn  ("x"::String) ("a"::String)   ::  [String]
        w2 = T.splitOn   ("x"::Text) ("a"::Text)  ::  [Text]
        c1 = intercalate  ("a"::String) ([]::[String])   :: String
        c2 = T.intercalate  ("a"::Text) ([]::[Text])   ::Text
        d1 = intercalate' ("a"::String) ([]::[String])   ::Maybe String
        d2 = intercalate' ("a"::Text) ([]::[Text])   ::Maybe Text
    putIOwordsS ["splitOn on empty input \nfor string", show r1]
    putIOwords ["\nfor text", show  r2, "--"]
    putIOwordsS ["S.splitOn on empty input \nfor string", show v1] --  ["","a"]
--    putIOwords ["\nT. for text", show  v2, "--"] -- produces error
    putIOwordsS ["S.splitOn on x input \nfor string", show w1] --  ["","a"]
    putIOwords ["\nT. for text", show  w2, "--"] -- produces error
    putIOwords ["intercalate for string",  c1, "-"]
    putIOwords ["intercalate for text",   t2s c2, "-"]
    putIOwords ["intercalate' for string", show  c1, "-"]
    putIOwords ["intercalate' for text",   show  c2, "-"]
    return True


putIOwordsT ::  MonadIO m => [T.Text] -> m ()
putIOwordsT = putIOwords
putIOwordsS :: MonadIO m =>  [String] -> m ()
putIOwordsS = putIOwords

unlinesT :: [T.Text] -> T.Text
unlinesT = unlines'

sortCaseInsensitive = sortBy cmpCaseInsensitive

cmpCaseInsensitive s1 s2 =  compare  (  toLower' s1) (  toLower' s2)

maybe2string :: (IsString s) =>  Maybe s -> s
maybe2string Nothing = ""  -- TODO
maybe2string (Just s) = s

string2maybe :: (Eq a, IsString a) => a -> Maybe a
string2maybe x = if x == "" then Nothing else Just x


-- tests that text operations have same semantics than string2maybe    putIOwords  =liftIOstrings
prop_unwords :: [String]  -> Bool
prop_unwords =     unwords' . map s2t  <=>  s2t . unwords

prop_words =  words' . s2t <=> map s2t . words

prop_unlines = unlines' . map s2t <=>  s2t . unlines
prop_lines = lines' . s2t <=> map s2t . lines

prop_append :: String -> String -> Bool
prop_append a b = append' (s2t a) (s2t b) == s2t  (append a  b)

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

prop_isPrefixOf a b = isPrefixOf' (s2t a) (s2t b) == isPrefixOf' a b
prop_isInfixOf' a b = isInfixOf' (s2t a) (s2t b) == isInfixOf' a b
prop_stripPrefix'f a b = stripPrefix' (s2t a) (s2t b) == fmap s2t  (stripPrefix' a b)

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
prop_splitOn_bytestring a b = splitOn' (s2b a) (s2b b)  == fmap (map s2b) (splitOn' a b)

prop_splitOn_intercalate :: String -> [String] -> Bool
prop_splitOn_intercalate a b =
    if null a then True
        else if any (a `isInfixOf'`) b then True
                else    (maybe True  (b==))  (maybe Nothing (splitOn' a)   ( intercalate' a $ b))
        -- fails on "" [""]

test_splitOn = assertBool ( Just [] == splitOn' [] ("a"::String))
test_b2s = assertEqual ("a"::String) (toString . t2b $ ("a" :: Text))

