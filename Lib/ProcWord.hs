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
import           Uniform.Strings
import           Uniform.FileIO

textlinesFile :: TypedFile5 Text [Text]
-- txtFile = makeTyped txtExtension
textlinesFile = makeTyped txtExtension
txtExtension :: Extension
txtExtension = Extension "txt" :: Extension

procWord2 :: [Text] -> Text -> Text
-- replace umlaut unless it is an permitted group
procWord2 erlaubt word =
  if checkErlaubt erlaubt word then word else procWord1 word

procWord1 :: Text -> Text
-- ^ convert the umlaut in a single word
-- no test, no exclusions
-- preserve capitalization
procWord1 t =
  replace' "AE" "Ä"
    . replace' "OE" "Ö"
    . replace' "UE" "Ü"
    . replace' "Ae" "Ä"
    . replace' "Oe" "Ö"
    . replace' "Ue" "Ü"
    . replace' "ae" "ä"
    . replace' "oe" "ö"
    . replace' "ue" "ü"
    $ t

erlaubt1 = map toLower' ["koef", "poet", "poes", "neue", "freue"] :: [Text]  -- erlaubte Gruppen - ergaenzen!

checkErlaubt :: [Text] -> Text -> Bool
-- ^ enthaelt das Wort eine erlaubte kombination
checkErlaubt erlaubt word = any (\e -> isInfixOf' e . toLower' $ word) erlaubt
checkErlaubt1 = checkErlaubt erlaubt1
-- mit fester Liste der erlaubten

readErlaubt :: Path Abs File -> ErrIO [Text]
-- read the erlaubte words wtih ae, oe and ue
readErlaubt fnErl = do
  erl :: [Text] <- read8 fnErl textlinesFile -- reads lines
  let erl2 = erl -- concat . map words' $ erl :: [Text]
  return erl2
instance TypedFiles7 Text [Text] where
  wrap7 t = words' t
  unwrap7 t = unwords' t
