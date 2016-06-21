-----------------------------------------------------------------------------
--
-- Module      :  StringInfix

-- infix operations with <X> to insert X in between - unconditional
-- even if the two strings are empty
-- todo - how to compare with similar ops in fileio?

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
    , NoMonomorphismRestriction
    #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.StringInfix  where


--import Data.Char (toUpper, toLower, isSpace)
import Control.Monad.State (MonadIO, liftIO)

import GHC.Exts( IsString(..) )
import qualified Data.Text as T
import Data.List as L
--import Data.List (isInfixOf, sortBy, stripPrefix, isPrefixOf)
import Data.StringConversion
import Data.StringUtilities

-- | a synonym for append
(<>) :: (Strings s) => s -> s -> s
(<>) = append'


-- (<:>), (<+>), (<->), (</>), spitz
(<:>) :: (IsString s, Strings s) => s -> s -> s
-- ^ append text with colon in between
a <:> b = a  <> ":" <> b

(<+>) :: (IsString s, Strings s) => s -> s -> s
-- append text with plus in between
a <+> b = a  <> "+" <> b

(<->) :: (IsString s, Strings s) => s -> s -> s
-- ^ append text with dash in between
a <-> b = a  <> "-" <> b

(</>) :: (IsString s, Strings s) => s -> s -> s
-- ^ append text with slash in between
a </> b = a  <> "/" <> b

a <.> b = a  <> "." <> b
-- ^ append text with dot in between

wrapInSpitz :: (IsString s, Strings s) => s -> s
-- ^ insert text in <..>
wrapInSpitz a = "<"  <> a <>  ">"

wrapInDoubleQuotes :: (IsString s, Strings s) => s -> s
-- ^ insert text in <..>
wrapInDoubleQuotes a = "\""  <> a <>  "\""

addXatEnd :: (IsString s, Strings s) => s -> s -> s
addXatEnd = flip append


