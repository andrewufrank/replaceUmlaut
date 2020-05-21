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

procWord :: Text -> Text
-- convert the umlaut in a single word
-- no test, no exclusions
-- preserve capitalization
procWord t =  replace' "AE" "Ä"
          . replace' "OE" "\196"
            . replace' "UE" "Ü"
            . replace' "Ae" "Ä"
          . replace' "Oe" "\196"
            . replace' "Ue" "Ü"
            . replace' "ae" "ä"
          . replace' "oe" "ö"
            . replace' "ue" "ü" $ t
