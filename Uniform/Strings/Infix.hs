-----------------------------------------------------------------------------
--
-- Module      :  StringInfix

-- infix operations with <X> to insert X in between - unconditional
-- even if the two strings are empty
-- todo - how to compare with similar ops in fileio?

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Strings.Infix  where


--import Data.Char (toUpper, toLower, isSpace)
-- import "monads-tf" Control.Monad.State (MonadIO, liftIO)

import           GHC.Exts                 (IsString (..))
import           Uniform.Strings.Conversion
import           Uniform.Strings.Utilities

---- | a synonym for append for text only
--(<>) :: (CharChains s) => s -> s -> s
----(<>) :: Text -> Text -> Text
--(<>) = append'

---- | a synonym for appendTwo
--(<>) :: (ListForms s) => s -> s -> s
----(<>) :: Text -> Text -> Text
-- (<>) = appendTwo  


-- (<:>), (<+>), (<->), (</>), spitz
--(<:>) :: (IsString s, CharChains s) => s -> s -> s
(<:>) :: Text -> Text -> Text
-- ^ append text with colon in between
a <:> b = a  <> ":" <> b

--(<+>) :: (IsString s, CharChains s) => s -> s -> s
(<+>) :: Text -> Text -> Text
-- append text with plus in between
a <+> b = a  <> "+" <> b

--(<->) :: (IsString s, CharChains s) => s -> s -> s
(<->) :: Text -> Text -> Text
-- ^ append text with dash in between
a <-> b = a  <> "-" <> b

--(</>) :: (IsString s, CharChains s) => s -> s -> s
(</>) :: Text -> Text -> Text
-- ^ append text with slash in between
a </> b = a  <> "/" <> b

(<.>) :: Text -> Text -> Text
a <.> b = a  <> "." <> b
-- ^ append text with dot in between

(<#>) :: Text -> Text -> Text
-- append text with hash in between
a <#> b = a  <> "#" <> b

(<|>) :: Text -> Text -> Text
-- append text with blank in between, (a character does not work as name)
-- possible conflict with parsec
a <|> x = a  <> " " <> x

--wrapInSpitz :: (IsString s, CharChains s) => s -> s
wrapInSpitz :: Text -> Text
-- ^ insert text in <..>
wrapInSpitz a = "<"  <> a <>  ">"

--wrapInDoubleQuotes :: (IsString s, CharChains s) => s -> s
wrapInDoubleQuotes :: Text -> Text
-- ^ insert text in <..>
wrapInDoubleQuotes a = "\""  <> a <>  "\""

--wrapInDoubleQuotes :: (IsString s, CharChains s) => s -> s
wrapInBraces :: Text -> Text
-- ^ insert text in <..>
wrapInBraces a = "{"  <> a <>  "}"

addXatEnd :: (IsString s, CharChains s) => s -> s -> s
addXatEnd = flip append
