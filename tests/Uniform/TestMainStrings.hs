-----------------------------------------------------------------------------
--
-- Module      :   for automatic test
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where

--import Uniform.Strings
import           Test.Framework
import           Test.Framework
import  {-@ HTF_TESTS @-}         Uniform.Strings.Conversion_test
import  {-@ HTF_TESTS @-}         Uniform.Strings.Infix_test
import   {-@ HTF_TESTS @-}        Uniform.Strings.Utilities_test

main =  do
    putStrLn "LitFoundation.hs:\n"
    -- r <- htfMain htf_thisModulesTests  -- if local tests
    -- putStrLn ("tree tagger test:\n" ++ show r)
    r <- htfMain htf_importedTests
    -- putStrLn ("other tests t:\n" ++ show r)
    return ()


-- examples
