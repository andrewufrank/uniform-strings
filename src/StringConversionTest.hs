-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

    {-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
    , UndecidableInstances
--    , OverloadedStrings
    , TypeFamilies

    #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import Test.Framework
import {-@ HTF_TESTS @-} Data.StringConversion
import {-@ HTF_TESTS @-} Data.StringUtilities

main = do
    putIOwords ["HTF LayoutTest.hs:\n posTest"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end LayoutTest.hs:\n posTest"]
    return r






