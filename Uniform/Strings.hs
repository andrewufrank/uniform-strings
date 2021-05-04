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
    , putIOlineList, putIOline
    , wordwrap
    , NiceStrings (..)
    , IsString (..)
--    , wordsT,  concatT, showT
--    , unlinesT, unwordsT,
--    , stringTest
    , module Uniform.Zero
    , module Uniform.ListForm
    , ppShowList, ppShow
    )   where


import           "monads-tf" Control.Monad.State      (MonadIO, liftIO)
import           Data.List                as L
import qualified Data.List.Split          as S
--if these string ops are desired (and not the usual ones from fileio
-- then import them from Data.StringInfix

-- only to avoid unusable shadowed
import           Data.String
import qualified Data.Text                as T

import qualified Data.Text.IO             as T 
import           Uniform.ListForm
import Text.Show.Pretty -- fromOthers (changed)
-- for the tests
import           Uniform.Strings.Conversion hiding (S)
import           Uniform.Strings.Infix  hiding ((<.>), (</>))
import           Uniform.Strings.Utilities
import           Uniform.Zero


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
--
putIOwordsT ::  MonadIO m => [T.Text] -> m ()
putIOwordsT = putIOwords

text0 = "" :: Text 

putIOline ::(MonadIO m, Show a) => Text -> a -> m () 
putIOline msg a = putIOwords [msg, showT a, "\n"] 
putIOlineList ::(MonadIO m, Show a) => Text -> [a] -> m () 
putIOlineList msg a = putIOwords [msg, unlines' . map showT $ a, "\n"] 
