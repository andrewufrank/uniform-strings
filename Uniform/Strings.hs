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
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Strings (
    module Uniform.Strings.Conversion
    , module Uniform.Strings.Utilities
    , module Uniform.Strings.Infix
    , module Data.String   -- for IsString class to make overloaded work
    , putIOwordsT
--    , putIOwordsS
    , putIOwords
    , wordwrap
    , NiceStrings (..)
    , IsString (..)
--    , wordsT,  concatT, showT
--    , unlinesT, unwordsT,
--    , stringTest
    , module Uniform.Zero
    , module Uniform.ListForm
    )   where


import           Uniform.Strings.Conversion
import           Uniform.Strings.Infix
-- hidde when conflict in use                    hiding ((<.>), (</>))
import           Uniform.Strings.Utilities
--if these string ops are desired (and not the usual ones from fileio
-- then import them from Data.StringInfix

-- only to avoid unusable shadowed
import Uniform.Zero
import Uniform.ListForm

import           "monads-tf" Control.Monad.State      (MonadIO, liftIO)
import           Data.String

-- for the tests
import           Data.List                as L
import qualified Data.List.Split          as S
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

putIOwords :: MonadIO m =>  [Text] -> m ()
putIOwords = liftIO . T.putStrLn. unwordsT

-- split a text into lines such that words are maintained
-- source https://gist.github.com/yiannist/4546899 - adapted
wordwrap :: Int -> Text -> [Text]
wordwrap maxWidth text' = map s2t $ reverse (lastLine : accLines)
  where
        text = t2s text'
        (accLines, lastLine) = foldl handleWord ([], "") (words text)
        handleWord (acc, line) word
          -- 'word' fits fine; append to 'line' and continue.
          | length line + length word <= maxWidth = (acc, word `append` line)
          -- 'word' doesn't fit anyway; split awkwardly.
          | length word > maxWidth                =
            let (line', extra) = splitAt maxWidth (word `append` line)
            in  (line' : acc, extra)
          -- 'line' is full; start with 'word' and continue.
          | otherwise                             = (line : acc, word)
        append word ""   = word
        append word line = line ++ " " ++  word
--class StringConvenience a where
--    putIOwords :: MonadIO m =>  [a] -> m ()
--    -- convenience function for simple output formating
----instance StringConvenience Text where
----    putIOwords  =liftIOstrings
--instance StringConvenience Text where
--    putIOwords = liftIOstrings . map T.unpack
----instance StringConvenience ByteString where
----    putIOwords = liftIOstrings . map b2s


--liftIOstrings ::MonadIO m => [String] -> m ()
--liftIOstrings   = liftIO . T.putStrLn . T.unwords
--
putIOwordsT ::  MonadIO m => [T.Text] -> m ()
putIOwordsT = putIOwords
--putIOwordsS :: MonadIO m =>  [String] -> m ()
--putIOwordsS = putIOwords
