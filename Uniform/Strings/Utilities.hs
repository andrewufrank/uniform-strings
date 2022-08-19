 --------------------------------------------------------------------
--
-- Module      :  Strings
-- Copyright   :
--
-- | a module with a class for strings, such that the normal functions are
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
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PackageImports  #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Strings.Utilities
    (module Uniform.Zero
    , module Uniform.ListForm
    , CharChains (..)
    , CharChains2 (..)
    , NiceStrings (..)
    , unlinesT, unwordsT
    , wordsT,  concatT, showT, readNoteT, readNoteTs
    , sortCaseInsensitive, cmpCaseInsensitive
    , maybe2string
    , showList'
    , putIOwords, debugPrint 
    , T.toTitle
    , toLowerStart, toUpperStart   
    , prop_filterChar
    , isSpace, isLower
    , PrettyStrings (..)
    -- to generalize
    , dropWhile, takeWhile, span, break
    , formatInt
    )
    where

import Uniform.Zero (Zeros(..))
import Uniform.ListForm ( ListForms(..) )


import           Data.Char                (isSpace, isLower, toLower, toUpper)
import           Text.Printf              (PrintfArg, PrintfType, printf)

import Data.List as L
    ( sortBy, intercalate, isInfixOf, isPrefixOf, nub, stripPrefix, intersperse )
import           GHC.Exts                 (IsString (..))

import qualified Data.List.Split         as S (splitOn)
import Data.Maybe ( catMaybes )
-- import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T (head, cons, tail, append, singleton, unwords, words, unlines, lines, empty, toUpper, toLower, concat, isPrefixOf, isInfixOf, stripPrefix, stripSuffix, intercalate, splitOn, strip, dropEnd, reverse, length, filter, take, drop, replace, null, toTitle)
import qualified Data.List.Utils       as LU (replace)
import Safe ( readNote )
import Uniform.Strings.Conversion
    ( Text,
      BSUTF,
      LazyByteString,
      s2t,
      t2s,
      t2bu,
      bu2t,
      bu2s,
      t2b,
      s2bu,
      b2bl )
import qualified Data.ByteString.Lazy as Lazy (append, length, take, drop)
import          Text.Read (readMaybe)
import Text.Show.Pretty ( ppShow ) 
import "monads-tf" Control.Monad.State      (MonadIO, liftIO)
import Control.Monad (when)
import Data.ByteString (intersperse)


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
toLowerStart t = (toLower . T.head $ t) `T.cons` T.tail t

toUpperStart :: Text -> Text
-- ^ convert the first character to Uppercase - for  PosTags in Spanish
toUpperStart t = (toUpper . T.head $ t) `T.cons` T.tail t

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

putIOwords :: MonadIO m =>  [Text] -> m ()
putIOwords = liftIO . putStrLn . t2s . unwords'

debugPrint :: (MonadIO m) => Bool -> [Text] -> m ()
-- ^ print the texts when the bool is true (flag debug)
debugPrint flag texts = when flag $ putIOwords texts 


--instance Zeros String where zero = (""::String)
instance Zeros Text where zero = "" :: Text

instance ListForms Text where
    type LF Text = Char
    appendTwo = T.append
    mkOne = T.singleton

instance ListForms String where
    type LF String = Char
    appendTwo = (++)
    mkOne = show

instance ListForms LazyByteString where
    type LF LazyByteString = Char
--    appendTwo = Lazy.append
    mkOne = b2bl . t2b . T.singleton

instance ListForms BSUTF where
    type LF BSUTF = Char
--    appendTwo a b =  t2bu . appendTwo  (bu2t a) $ bu2t b
    mkOne =  t2bu . T.singleton

--instance Zeros BSUTF where   -- derived in Conversion
--    zero = t2bu ""

class (Zeros a, ListForms a, Eq a) => CharChains a where
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
    append', append   :: a -> a -> a  -- duplication?
    append = appendTwo
    append' = appendTwo

    null' :: a -> Bool
    null' = isZero
    mknull :: a
    toLower' :: a -> a
    -- ^ convert the string to  lowercase, idempotent
    -- is not inverse of toUpper
    toUpper':: a -> a
    -- ^ is not idempotent and gives different results for string and text (sz and similar ligatures)

    isPrefixOf', isInfixOf', isPostfixOf' :: a -> a -> Bool
    isPostfixOf' a = isPrefixOf' (reverseString a) . reverseString
    stripPrefix' :: a -> a -> Maybe a
    -- ^ takes the prefix away, if present (and return rest). nothing if no prefix
    stripSuffix' :: a -> a -> Maybe a
    concat' :: [a] -> a
    trim' :: a -> a
    -- ^ removes all spaces front and back, idempotent
    reverseString, reverse' :: a -> a
    reverse' = reverseString
    removeLast :: a -> a
    -- ^ remove last char
    removeChar ::Char -> a -> a
    -- ^ remove a character from a string
    filterChar:: (Char -> Bool)  -> a -> a
    -- filter lets pass what is true
    lengthChar :: a -> Int
    nubChar :: a -> a
    drop' :: Int -> a -> a
    -- drop n char from input start
    take' :: Int -> a -> a
    -- ^ add a splitAt or dropN function
    intercalate' :: a -> [a] -> Maybe a
    -- ^ splitOn' and intercalate' are inverses (see Data.SplitList)
    -- returns Nothing if second  is empty and intercalate "x" "" gives Just ""
    -- return Nothing if first is empty or contained in second to achievee inverse with splitOn
    splitOn' :: a -> a -> Maybe [a]
    -- ^ splits the first by all occurences of the second 
    -- the second is removed from results
    -- returns Nothing if second is empty

    printf' :: (PrintfArg r) => String -> r -> a
    -- ^ formats a string accoding to a pattern - restricted to a single string (perhaps)
    -- requires type of argument fixed!

--    length' :: a -> Int
    replace' :: a -> a -> a -> a
    -- replace the first string with the second string in the third string
    readMaybe' :: Read b => a -> Maybe b
    -- read something... needs type hints

    prop_filterChar :: a -> Bool
    -- test with fixed set of chars to filter out

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
    null' = null
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
        | s `isInfixOf` concat' a = Nothing
        | otherwise = Just $ L.intercalate s a

    splitOn' o s
        | null' o = Just []
        | null' s = Just [""]
        | otherwise = Just $ S.splitOn o s
    trim' = f . f
        where f = reverse . dropWhile isSpace
    removeLast a =  if null' a
        then mknull
        else reverseString . tail . reverseString $ a
    reverseString = reverse
    printf' = printf
    lengthChar = length
    removeChar c = filter (c /=)
    filterChar = filter

    nubChar = nub
    take'  = take
    drop' = drop
    replace' = LU.replace
    readMaybe' = readMaybe

instance CharChains Text where
    toString = t2s
    toText = id

    unwords' = T.unwords
    words' =  T.words
    lines' = T.lines
    unlines' = T.unlines
    mknull = T.empty
    toUpper' = T.toUpper
    toLower' = T.toLower
    concat' = T.concat
    isPrefixOf' = T.isPrefixOf
    isInfixOf' = T.isInfixOf
    stripPrefix' = T.stripPrefix
    stripSuffix' = T.stripSuffix

    intercalate' s a
      | null a = Nothing :: Maybe Text
      | null' s = Nothing
      | s `isInfixOf'` concat' a = Nothing
      | otherwise = Just $ T.intercalate s a

    splitOn' o s
      | null' o = Just []
      | null' s = Just [""]
      | otherwise = Just $ T.splitOn o s

    trim' = T.strip 
    removeLast a = T.dropEnd 1 a

    reverseString = T.reverse  

    printf' p   = s2t . printf p
    lengthChar = T.length
    removeChar c = T.filter (c /=)
    filterChar = T.filter
    nubChar = s2t . nub .t2s
    take' = T.take
    drop' = T.drop

    prop_filterChar a = t2s af == (filterChar cond . t2s $ a)
      where
          cond x = x `notElem` ['a', '\r', '1']
          af = filterChar cond a :: Text
    replace' = T.replace
    readMaybe' = readMaybe' . t2s

instance CharChains LazyByteString where
    append' = Lazy.append
    lengthChar a = fromIntegral . Lazy.length $ a  
        --  gives not exact value??
    take' = Lazy.take . fromIntegral 
    drop' = Lazy.drop . fromIntegral  

unwordsT :: [Text] -> Text
unwordsT = T.unwords  
-- ^ to fix types for overloaded strings

wordsT :: Text -> [Text]
wordsT = words'

concatT ::  [Text] -> Text
concatT = concat'

showT t = s2t c
    where c = show t :: String

instance CharChains BSUTF  where
-- works on utf8 encoded bytestring, convert with b2bu and bu2b

    toString = bu2s
    toText = bu2t

    unwords' = t2bu . unwords' . map bu2t
    words' =  map t2bu .  words' . bu2t
    lines' = map t2bu . lines' . bu2t
    unlines' =  t2bu . unlines' . map bu2t

--    append' a b = t2bu . append' (bu2t a) $ bu2t b
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

formatInt :: Int -> Int -> Text
formatInt n  = s2t . case n of
        6 -> printf  ['%', '0', '6', 'd']
        5 ->  printf  ['%', '0', '5', 'd']
        3 -> printf  ['%', '0', '3', 'd']
        2 ->  printf  ['%', '0', '2', 'd']
        a -> error ("formatInt not expected int" <> show a)

unlinesT :: [Text] -> Text
unlinesT = unlines'

sortCaseInsensitive :: (Ord a, CharChains a) => [a] -> [a]
sortCaseInsensitive = sortBy cmpCaseInsensitive

cmpCaseInsensitive :: (Ord a, CharChains a) => a -> a -> Ordering
cmpCaseInsensitive s1 s2 =  compare  (  toLower' s1) (  toLower' s2)

maybe2string :: (IsString s) =>  Maybe s -> s
maybe2string Nothing  = ""  -- TODO
maybe2string (Just s) = s

string2maybe :: (Eq a, IsString a) => a -> Maybe a
string2maybe x = if x == "" then Nothing else Just x

class  (Show a) =>  NiceStrings a where
    shownice, showNice :: a -> Text
    -- showNice = shownice
    shownice = showT  -- as default 
    showlong :: a -> Text
    showAsLines :: [a] -> Text
    -- ^ show on a line, does not propagate, inside is shown normally
    showAsLines = unlines' . map showT 
    showlong = shownice  -- a default
class Show a => PrettyStrings a where 
    showPretty :: a -> Text
instance  {-# OVERLAPPABLE #-} Show a => PrettyStrings a where
    showPretty = s2t . ppShow

instance NiceStrings Text where
    shownice = id
    showlong = id

instance NiceStrings Int where
    shownice = show'
    showlong = show'

-- instance NiceStrings Float where shownice = s2t . showDP 4

-- for printf https://hackage.haskell.org/package/base-4.17.0.0/docs/Text-Printf.html
instance NiceStrings Float where shownice f = s2t $ printf "%.3f" f

instance NiceStrings Double where
    shownice s = s2t . printf "%.3f" $ s
    showlong = show'

instance (NiceStrings a, NiceStrings b) => NiceStrings (a,b) where
    shownice (a,b) = unwords' [shownice a, shownice b]
    showlong (a,b) = unwords' [showlong a, showlong b]
instance (Show a, NiceStrings a) => NiceStrings [a] where
    shownice as = concat' . L.intersperse ",\t\t" $ (map shownice as) 
    showlong as = concat' . L.intersperse ",\n" $ (map shownice as) 
    -- catMaybes $ [intercalate' "\n" .  map showlong $ as, Just "\n"]

instance (NiceStrings a) => NiceStrings (Maybe a) where
    shownice (Just a)  = shownice a
    shownice Nothing = "Nothing"

instance (Show a, Show b, Show c) => NiceStrings (a,b,c) where 
            showNice a = showT a 

