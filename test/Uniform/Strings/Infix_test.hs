-----------------------------------------------------------------------------
--
-- Module      :  StringInfix

-- infix operations with <X> to insert X in between - unconditional
-- even if the two strings are empty
-- todo - how to compare with similar ops in fileio?

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Strings.Infix_test  where


--import Data.Char (toUpper, toLower, isSpace)
-- import "monads-tf" Control.Monad.State (MonadIO, liftIO)

import           GHC.Exts                 (IsString (..))
import           Test.Framework
import           Uniform.Strings.Conversion
import           Uniform.Strings.Utilities


