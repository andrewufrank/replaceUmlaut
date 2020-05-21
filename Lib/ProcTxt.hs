-----------------------------------------------------------------------------
-- Module      :   Convert umlaut written as ae, oe or ue into ä, ö and ü
--              in a txt file
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.ProcTxt  -- (openMain, htf_thisModuelsTests)
     where
-- import Uniform.Strings
-- import Uniform.TypedFile
import Uniform.FileIO
-- import Uniform.Error
import Lib.ProcWord

txtFile   = makeTyped (Extension "txt")

procTxt :: [Text] -> Path Abs File -> ErrIO ()
-- ^ replace umlaut unless it is an permitted group
-- in a file with extension txt
procTxt erlaubt fn = do

    ls <- read6 fn txtFile
    let ls2 = map (procLine erlaubt) ls
    cdir <- currentDir
    let fn2 = cdir </> makeRelFile "TestKorrekted" :: Path Abs File

    write6 fn2 txtFile ls2

procLine :: [Text] -> Text -> Text
procLine erlaubt ln = unwords' . map (procWord2 erlaubt) . words' $ ln
-- process all words in a line
