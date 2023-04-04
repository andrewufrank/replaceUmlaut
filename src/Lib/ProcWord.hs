-----------------------------------------------------------------------------
-- Module      :   Convert umlaut written as ae, oe or ue into ä, ö and ü
--              deals with lines, preserving the leading spaces and tabs.
-- could be improved to use span to break on first non-space character
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
-- import           Uniform.Strings
-- import           Uniform.FileIO
import UniformBase
import Lib.FileHandling
import qualified Data.Text   as T



-- procTxt2 :: [Text] ->  Text -> Text  -- called from OneMDfile, not from ProcTxt
-- -- change all umlaut in text - yaml header and markdown text
-- -- preserve leading blanks of line, or tabs, but not mixture of these
-- procTxt2 erl2  = unlines' . map (procLine2 erl2) . lines' 


procLine2 :: [Text] ->  Text -> Text
-- process one line preserving spaces or tabs (but not a mix) at start
-- improve to use span break on first non-space 
procLine2 erl2 t = concat' [ld,procLine erl2 t1]
    where
        (ld, t1) = case mb1 t of
                Nothing -> case mb2 t of 
                                Nothing -> ("", t)
                                Just (lead2, _, t02) -> (lead2,t02)
                Just (lead, _, t0) ->  (lead,t0)
        mb1 tx = T.commonPrefixes "                  " tx
        mb2 ty = T.commonPrefixes "\t\t\t\t\t\t\t" ty

procLine :: [Text] -> Text -> Text
procLine erlaubt ln = unwords' . map (procWord2 erlaubt) . words' $ ln
-- process all words in a line
-- should be idempotent 

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

erlaubt1 :: [Text]  -- erlaubte Gruppen - for test only
erlaubt1 = map toLower' ["koef", "poet", "poes", "neue", "freue"] 

checkErlaubt :: [Text] -> Text -> Bool
-- ^ enthaelt das Wort eine erlaubte kombination
checkErlaubt erlaubt word = any (\e -> isInfixOf' e . toLower' $ word) erlaubt

checkErlaubt1 :: Text -> Bool 
checkErlaubt1 = checkErlaubt erlaubt1
-- mit fester Liste der erlaubten - for test

