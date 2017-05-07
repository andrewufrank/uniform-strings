-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-- do not prettify
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework
import  {-@ HTF_TESTS @-}         Uniform.StringConversion
import  {-@ HTF_TESTS @-}         Uniform.Strings
import   {-@ HTF_TESTS @-}        Uniform.StringUtilities

main = do
    putIOwords ["HTF UniformStringTest.hs:\n"]
--    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    r <- htfMain  htf_importedTests
    putIOwords ["HTF end UniformSringTest.hs:\n"]
    return r
