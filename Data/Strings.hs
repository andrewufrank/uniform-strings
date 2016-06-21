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
{-# LANGUAGE
    MultiParamTypeClasses
--    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
--    , FlexibleContexts
--    , DeriveFunctor
--    , ScopedTypeVariables
--    , OverlappingInstances
    , UndecidableInstances
    , OverloadedStrings
--    , TypeFamilies
--    , NoMonomorphismRestriction
    #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.Strings (
    module Data.StringConversion
    , module Data.StringUtilities
    , module Data.StringInfix
    )   where


--import qualified Data.Text as T (Text)
import Data.StringConversion
import Data.StringUtilities
import Data.StringInfix hiding ((</>), (<.>))
--if these string ops are desired (and not the usual ones from fileio
-- then import them from Data.StringInfix



