{-  -- Module      :   Convert umlaut written as ae, oe or ue into ä, ö and ü
--              in a single word
-----------------------------------------------------------------------------
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

import Uniform.Strings

module Lib.ProcWord  -- (openMain, htf_thisModuelsTests)
     where
procWord :: Text -> Text
-- convert the umlaut in a single word
-- no test, no exclusions
-- preserve capitalization
procWord t = replace' "ae" "ä" t
