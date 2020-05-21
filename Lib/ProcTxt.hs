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
import           Uniform.FileIO
-- import Uniform.Error
import           Lib.ProcWord

txtFile :: TypedFile5 [Text] a
txtFile = makeTyped (Extension "txt")
-- ^ filetype to read text in lines

-- procTxt2 :: Path Abs File -> Path Abs File -> ErrIO ()
-- -- ^ replace umlaut in file2 except if contained
-- --  in the list (first File Path)
-- procTxt2 fnErl fn = do
--     erl <- read6 fnErl txtFile
--     let erlaubt = concat . map words' $ erl ::[Text]
--     procTxt erlaubt fn

procTxt :: Path Abs File -> Path Abs File -> ErrIO ()
-- ^ replace umlaut unless it is an permitted group
-- in a file with extension txt
procTxt fnErl fn = do
  erl <- read6 fnErl txtFile -- reads lines
  let erlaubt = concat . map words' $ erl :: [Text]

  ls :: [Text] <- read6 fn txtFile

  let ls2      = map (procLine erlaubt) ls

  let fnrename = fn <.> (Extension "bak") :: Path Abs File
  renameOneFile (fn <.> (Extension "txt")) fnrename
  putIOwords ["procTxt renamed", showT fnrename]
  write6 fn txtFile ls2

procLine :: [Text] -> Text -> Text
procLine erlaubt ln = unwords' . map (procWord2 erlaubt) . words' $ ln
-- process all words in a line
