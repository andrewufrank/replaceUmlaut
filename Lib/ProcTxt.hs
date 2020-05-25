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

bakExtension :: Extension
bakExtension = Extension "bak" :: Extension
-- ^ filetype to read text in lines

-- procTxt2 :: Path Abs File -> Path Abs File -> ErrIO ()
-- -- ^ replace umlaut in file2 except if contained
-- --  in the list (first File Path)
-- procTxt2 fnErl fn = do
--     erl <- read6 fnErl txtFile
--     let erlaubt = concat . map words' $ erl ::[Text]
--     procTxt erlaubt fn

procTxt :: Bool -> Path Abs File -> Path Abs File -> ErrIO ()
-- ^ replace umlaut unless it is an permitted group
-- in a file with extension txt
procTxt debug fnErl fn = do
  when debug $ putIOwords ["procTxt start", showT fn]
  erl2         <- readErlaubt fnErl
  -- erl  <- read6 fnErl txtFile -- reads lines
  -- let erlaubt = concat . map words' $ erl :: [Text]

  ls :: [Text] <- read8 fn textlinesFile

  let ls2 = map (procLine erl2) ls
  let res = zero -- not needed, just to be parallel with pandoc

  -- rest is copied
  if debug
    then do
      let fnnew = makeAbsFile (toFilePath fn <> "NEW")
      putIOwords ["procMd result in new", showT fnnew]
      write8 fnnew textlinesFile res
      putIOwords ["procMd result in new written", showT res, "fn", showT fnnew]
    else do
      let fnrename = fn <.> bakExtension :: Path Abs File
      renameOneFile (fn <.> txtExtension) fnrename
      putIOwords ["procMd renamed to bak", showT fnrename]
  write8 fn textlinesFile ls2

procLine :: [Text] -> Text -> Text
procLine erlaubt ln = unwords' . map (procWord2 erlaubt) . words' $ ln
-- process all words in a line
