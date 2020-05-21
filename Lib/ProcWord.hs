-----------------------------------------------------------------------------
-- Module      :   Convert umlaut written as ae, oe or ue into ä, ö and ü
--              in a single word
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.ProcWord  -- (openMain, htf_thisModuelsTests)
     where
import Uniform.Strings
procWord2 :: [Text] -> Text -> Text
-- replace umlaut unless it is an permitted group
procWord2 erlaubt word =
    if checkErlaubt erlaubt word
      then word
      else procWord1 word

procWord1 :: Text -> Text
-- ^ convert the umlaut in a single word
-- no test, no exclusions
-- preserve capitalization
procWord1 t =  replace' "AE" "Ä"
          . replace' "OE" "\196"
            . replace' "UE" "Ü"
            . replace' "Ae" "Ä"
          . replace' "Oe" "\196"
            . replace' "Ue" "Ü"
            . replace' "ae" "ä"
          . replace' "oe" "ö"
            . replace' "ue" "ü" $ t

erlaubt1 = map toLower' ["koef", "poet", "poes", "neue", "freue"] :: [Text]  -- erlaubte Gruppen - ergaenzen!

checkErlaubt :: [Text] -> Text -> Bool
-- ^ enthaelt das Wort eine erlaubte kombination
checkErlaubt erlaubt word = any (\e -> isInfixOf' e . toLower'  $ word) erlaubt
checkErlaubt1 = checkErlaubt erlaubt1
-- mit fester Liste der erlaubten
