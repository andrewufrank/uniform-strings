-----------------------------------------------------------------------------
--
-- Module      :  StringInfix

-- infix operations with <X> to insert X in between - unconditional
-- even if the two strings are empty
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Strings.Infix  where

import           GHC.Exts                 (IsString (..))
import Uniform.Strings.Conversion ( Text )
import Uniform.Strings.Utilities ( CharChains(append) )


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
-- ^ append text with blank in between, (a character does not work as name)
-- possible conflict with parsec
a <|> x = a  <> " " <> x

wrapInSpitz :: Text -> Text
-- ^ insert text in <..>
wrapInSpitz a = "<"  <> a <>  ">"

wrapInDoubleQuotes :: Text -> Text
-- ^ insert text in <..>
wrapInDoubleQuotes a = "\""  <> a <>  "\""

wrapInBraces :: Text -> Text
-- ^ insert text in <..>
wrapInBraces a = "{"  <> a <>  "}"

addXatEnd :: (IsString s, CharChains s) => s -> s -> s
addXatEnd = flip append
