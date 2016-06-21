-----------------------------------------------------------------------------
--Main.hs
-- Module      :  Main
-- Copyright   :  andrew u frank
--
-- | the simple string conversions
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE
    MultiParamTypeClasses
--    , TypeSynonymInstances
--    , FunctionalDependencies
--    , FlexibleInstances
--    , FlexibleContexts
--    , DeriveFunctor
    , ScopedTypeVariables
--    , UndecidableInstances
    , OverloadedStrings
    #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main (main) where

import Control.Monad (liftM, forever, when)

import qualified Data.Text as T (Text)
import Data.Strings


programName = "stringconversion-0.0.1"
versionNum =   "3191"

debug_main  =  True

stringTest = True

main :: IO ()
main = do
        putIOwordsT ["start  \n" ]
        putIOwordsT [ "------------------ ", programName
--                , toText versionNum
                , " -------------------------"]
        let r = stringTest
        putIOwordsT ["main", programName, "returning", toText r, "-------------------------\n\n"]
