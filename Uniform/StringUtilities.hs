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
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Uniform.StringUtilities
    (
    CharChains (..)
    , CharChains2 (..)
    , NiceStrings (..)
    , unlinesT, unwordsT
    , wordsT,  concatT, showT, readNoteT, readNoteTs
    , sortCaseInsensitive, cmpCaseInsensitive
    , maybe2string
    , htf_thisModulesTests
    , showList'
    , T.toTitle
    , toLowerStart, toUpperStart  -- for types and properties in RDF
    )
    where

import           Algebra.Laws             as Law
import           Test.Framework
import           Test.Invariant           as Rule

-- probably better just to move these module to package uniform-algebra
-- but there is so far only zero

import           Data.Char                (isSpace, isLower, toLower, toUpper)
import           Text.Printf              (PrintfArg, PrintfType, printf)

import           Data.List                as L
--import qualified Data.Vector              as V
import           GHC.Exts                 (IsString (..))

import qualified Data.List.Split          as S
import           Data.Maybe
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Safe
-- import           Uniform.Error            (fromJustNote)
-- not possible, because Error is based on String
import           Uniform.StringConversion
import qualified Data.ByteString.Lazy as Lazy
--
-- | generalized functions to work on chains of characters
-- (i.e. strings, text, url encoded, bytestring), text and bytestring
-- with the same semantics
-- change name to CharChains was Strings
--
readNoteTs :: (Show a, Read a) =>  [Text] -> Text -> a   -- TODO
-- ^ read a Text into a specific format
readNoteTs msg a = readNote  (unlines (map t2s msg) <> show a) . t2s $ a

readNoteT :: Read a =>  Text -> Text -> a   -- TODO
-- ^ read a Text into a specific format
readNoteT msg = readNote (t2s msg) . t2s

showList' :: Show a =>  [a] -> Text
-- ^ show a collection of lines
showList' = unlines' . map showT

toLowerStart :: Text -> Text
-- ^ convert the first character to lowercase - for Properties in RDF
toLowerStart t = (toLower . T.head $ t ) `T.cons` (T.tail t)

toUpperStart :: Text -> Text
-- ^ convert the first character to Uppercase - for  PosTags in Spanish
toUpperStart t = (toUpper . T.head $ t ) `T.cons` (T.tail t)

class (Eq a) => CharChains a where
--    {-# MINIMAL   #-}

    toString ::  a -> String
    toText :: Show a => a -> Text
    -- ^ conversion


    unwords' :: [a] -> a
    words' :: a -> [a]
    unlines' :: [a] -> a
    lines' :: a -> [a]
--    punwords :: [a] -> s
    toText = s2t . show
    append', append  :: a -> a -> a

    append = append' -- without ' to maintain old code

    null' :: a -> Bool
--    isLowerCase :: a -> Bool
--    isSpaceChar :: a -> Bool
-- operates on char, would be allLower?
    mknull :: a
--    -- to avoid a dependency on zero from algebra

    toLower' :: a -> a
    -- ^ convert the string to  lowercase, idempotent
    -- is not inverse of toUpper
    toUpper':: a -> a
    -- is not idempotent and gives different results for string and text (sz and similar ligatures)

    isPrefixOf', isInfixOf', isPostfixOf' :: a -> a -> Bool
    isPostfixOf' a = isPrefixOf' (reverseString a) . reverseString
    stripPrefix' :: a -> a -> Maybe a
    -- takes the prefix away, if present (and return rest). nothing if no prefix
    stripSuffix' :: a -> a -> Maybe a
    concat' :: [a] -> a
    trim' :: a -> a
    -- ^ removes all spaces front and back, idempotent
    reverseString :: a -> a
    removeLast :: a -> a
    -- ^ remove last char
    removeChar ::Char -> a -> a
    -- ^ remove a character from a string
    filterChar:: (Char -> Bool)  -> a -> a
    -- filter lets pass what is true
    lengthChar :: a -> Int
    nubChar :: a -> a
    take' :: Int -> a -> a
    -- add a splitAt or dropN function
    -- repalceAll function from POS
    intercalate' :: a -> [a] -> Maybe a
    -- ^ splitOn' and intercalate' are inverses (see Data.SplitList)
    -- returns Nothing if second  is empty and intercalate "x" "" gives Just ""
    -- return Nothing if first is empty or contained in second to achievee inverse with splitOn
    splitOn' :: a -> a -> Maybe [a]
    -- ^ returns Nothing if second is empty

    printf' :: (PrintfArg r, PrintfType r) => String -> r -> a
    -- ^ formats a string accoding to a pattern - restricted to a single string (perhaps)
    -- requires type of argument fixed!
--    see http://hackage.haskell.org/package/base-4.9.1.0/docs/Text-Printf.html#v:printf
--       -      left adjust (default is right adjust)
--   +      always use a sign (+ or -) for signed conversions
--   space  leading space for positive numbers in signed conversions
--   0      pad with zeros rather than spaces

--    length' :: a -> Int

    prop_zero_mknull :: a -> Bool
    prop_zero_mknull a = Law.zero append' a mknull

    prop_assoz :: a -> a -> a -> Bool
    prop_assoz a b c = Rule.associative append' a b c

    prop_concat :: [a] -> Bool
    prop_concat as =    (concat' as)==(foldr append' mknull as)

    prop_filterChar :: a -> Bool
    -- test with fixed set of chars to filter out
--    prop_filterChar a = all (cond) . show $  af
--      where
--          cond x = x `notElem` ['a', '\r', '1']
--          af = (filterChar cond a)
class CharChains2 x a where
    show' ::  x -> a
-- replaced with toString or toText

instance CharChains2 Int String where
    show' = show
instance CharChains2 Bool String where
    show' = show
instance CharChains2 () String where
    show' = show
instance CharChains2 Int Text where
    show' = s2t . show
instance CharChains2 Bool Text where
    show' = s2t . show
instance CharChains2 () Text where
    show' = s2t . show

instance CharChains2 Float String where
    show'  = show
instance CharChains2 Float  Text where
    show'  = s2t . show

instance CharChains2 Double String where
    show'  = show
instance CharChains2 Double Text where
    show'  = s2t . show
instance CharChains2 Text Text where
    -- avoid the "" surrounding show text
    show'  = id
instance CharChains2 String Text where
    show'  = s2t
instance CharChains2 Text String where
    -- avoid the "" surrounding show text
    show'  = t2s
instance CharChains2 String String where
    show'  = id

instance (Show a, Show b) => CharChains2 (a,b) String where
    show' (a,b) = show (a,b)
instance (Show a, Show b) => CharChains2 (a,b) Text where
    show' (a,b) = s2t $ show (a,b)

instance (Show a) => CharChains2 [a] String where
    show' s = show s
instance (Show a) => CharChains2 [a] Text where
    show' s = s2t . show $ s

instance CharChains String where
    toString = id
    toText = s2t

    unwords' = unwords
    words' = words
    unlines' = unlines
    lines' = lines
    append' = (++)
    null' = null
--    isLowerCase = isLower
    mknull = ""
    toUpper' = map toUpper
    toLower' = map toLower
    concat' = concat
    isPrefixOf'  = isPrefixOf
    isInfixOf' = isInfixOf
    stripPrefix'  = stripPrefix
    stripSuffix' a  = fmap reverse . stripPrefix (reverse a) . reverse

    intercalate' s a
        | null a = Nothing
        | null s = Nothing
        | s `isInfixOf` (concat' a) = Nothing
        | otherwise = Just $ L.intercalate s a

    splitOn' o s
        | null' o = Just []
        | null' s = Just [""]
        | otherwise = Just $ S.splitOn o s
    trim' = f . f
        where f = reverse . dropWhile isSpace
    removeLast a =  if null' a
        then mknull
        else (reverseString . tail . reverseString $ a)
    reverseString = reverse
    printf' = printf
    lengthChar = length
    removeChar c = filter (c /=)
    filterChar = filter

    nubChar = nub
    take'  = take

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

--instance CharChains2 String String where
--    show' =  id
--instance (Show x ) => CharChains2 x String where
--    show' = show

instance CharChains Text where
    toString = t2s
    toText = id

    unwords' = T.unwords
    words' =  T.words
    lines' = T.lines
    unlines' = T.unlines
    append' = T.append
    null' = T.null
    mknull = T.empty
    toUpper' = T.toUpper
    toLower' = T.toLower
    concat' = T.concat
    isPrefixOf' = T.isPrefixOf
    isInfixOf' = T.isInfixOf
    stripPrefix' = T.stripPrefix
    stripSuffix' = T.stripSuffix

    intercalate' s a
      | null a = (Nothing :: Maybe Text)
      | null' s = Nothing
      | (s `isInfixOf'` (concat' a)) = Nothing
      | otherwise = Just $ T.intercalate s a

    splitOn' o s
      | null' o = Just []
      | null' s = (Just [""])
      | otherwise = Just $ T.splitOn o s

    trim' = T.strip -- s2t . trim' . t2s
--    splitOn' o s= if null' o then Just []
--                        else if null' s then Nothing else Just $ T.splitOn o s
    removeLast a = T.dropEnd 1 a
--        if null' a
--        then mknull
--        else (s2t . reverseString . tail . reverseString . t2s $ a)
    reverseString = T.reverse  -- s2t . reverseString . t2s

    printf' p   = s2t . printf p
    lengthChar = T.length
    removeChar c = T.filter (c /=)
    filterChar = T.filter
    prop_filterChar a = (t2s af) == (filterChar cond . t2s $ a)
      where
          cond x = x `notElem` ['a', '\r', '1']
          af = filterChar cond a :: Text
    nubChar = s2t . nub .t2s
    take' = T.take

instance CharChains LazyByteString where
    append' = Lazy.append

unwordsT :: [Text] -> Text
unwordsT = T.unwords  -- to fix types for overloaded strings

wordsT :: Text -> [Text]
wordsT = words'

concatT ::  [Text] -> Text
concatT = concat'

--showT ::(CharChains2 a Text) =>  a -> Text
--showT = show'
showT = s2t . show
--
prop_zero_mknull_text :: Text -> Bool
prop_zero_mknull_text = prop_zero_mknull

prop_assoz_text :: Text -> Text -> Text -> Bool
prop_assoz_text  = prop_assoz

prop_concat_text :: [Text] -> Bool
prop_concat_text  =  prop_concat

prop_filterChar_text :: Text -> Bool
prop_filterChar_text = prop_filterChar

instance CharChains BSUTF  where
-- works on utf8 encoded bytestring, convert with b2bu and bu2b
-- b2bu -> Maybe
-- doubtful - what is possible without error? t2b . b2t is not id
--  all achieved by transforming the input to text and operating on this.
-- result cannot be translated back
-- alternatively used Data.ByteString.Char8 -- which assumes that only 8 bit char

    toString = bu2s
    toText = bu2t

    unwords' = t2bu . unwords' . map bu2t
    words' =  map t2bu .  words' . bu2t
    lines' = map t2bu . lines' . bu2t
    unlines' =  t2bu . unlines' . map bu2t

    append' a b = t2bu . append' (bu2t a) $ bu2t b
    null' = T.null . bu2t
    toUpper' = t2bu . toUpper' . bu2t
    toLower' = t2bu . toLower' . bu2t
    concat' = t2bu . concat' . map bu2t
    isPrefixOf' t s  = isPrefixOf' (bu2t s) (bu2t t)
    isInfixOf' t s  = isInfixOf' (bu2t s) (bu2t t)
    stripPrefix' p s = t2bu <$> stripPrefix' (bu2t p)  (bu2t s)
    intercalate' x a  =  fmap t2bu  (intercalate' (bu2t x) (map bu2t a))
    splitOn' o s = fmap t2bu <$> splitOn' (bu2t o) (bu2t s)
    trim' = s2bu . trim' . bu2s

--prop_zero_mknull_bs :: ByteString -> Bool
--prop_zero_mknull_bs = prop_zero_mknull
-- no instance for arbitrary ByteString

--instance CharChains Int where
--    toString = show
--
--instance CharChains Bool where
--    toString = show

unlinesT :: [Text] -> Text
unlinesT = unlines'

sortCaseInsensitive = sortBy cmpCaseInsensitive

cmpCaseInsensitive s1 s2 =  compare  (  toLower' s1) (  toLower' s2)

maybe2string :: (IsString s) =>  Maybe s -> s
maybe2string Nothing  = ""  -- TODO
maybe2string (Just s) = s

string2maybe :: (Eq a, IsString a) => a -> Maybe a
string2maybe x = if x == "" then Nothing else Just x


class NiceStrings a where
-- ^ produce a text - any particular needs ? (otherwise replace with showT
-- the needs are to have a non-read-parse conversion
-- integrate in StringUtilities
    shownice :: a -> Text

instance NiceStrings Text where shownice = id

instance NiceStrings Int where shownice = show'
instance NiceStrings Double where shownice = show'

instance (NiceStrings a, NiceStrings b) => NiceStrings (a,b) where
    shownice (a,b) = unwords' [shownice a, shownice b]
instance (NiceStrings a) => NiceStrings [a] where
    shownice as = concat' . catMaybes $ [intercalate' "," .  map shownice $ as, Just "\n"]
--instance (NiceStrings a) => NiceStrings (V.Vector a) where
--    shownice  = unwords' . map shownice . V.toList


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
