 --------------------------------------------------------------------
--
-- Module      :  Strings
-- Copyright   :
--
-- | a top module exporting the operations in the package
----------------------------------------------------------------------
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
    , module Data.String    
    , putIOwords
    , NiceStrings (..)
    , IsString (..)
    , module Uniform.Zero
    , module Uniform.ListForm
    , ppShowList, ppShow
    )   where


import           "monads-tf" Control.Monad.State      (MonadIO, liftIO)
import Data.List as L ()
import qualified Data.List.Split          as S
--if these string ops are desired (and not the usual ones from fileio
-- then import them from Data.StringInfix

import Data.String
    ( IsString(..), lines, unlines, unwords, words, String )
import qualified Data.Text                as T

import qualified Data.Text.IO             as T 
import Uniform.ListForm ( ListForms(..) )
import Text.Show.Pretty ( ppShow, ppShowList ) -- fromOthers (changed)
-- for the tests
import           Uniform.Strings.Conversion hiding (S)
import           Uniform.Strings.Infix  hiding ((<.>), (</>))
import           Uniform.Strings.Utilities
import           Uniform.Zero

