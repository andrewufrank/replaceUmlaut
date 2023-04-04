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
-- import           Uniform.FileIO
-- import Uniform.Error
import           Lib.ProcWord
import UniformBase
import Lib.FileHandling

procTxt :: Bool -> [Text] -> Path Abs File -> ErrIO Bool
-- ^ replace umlaut unless it is an permitted group
-- in a file with extension txt
procTxt debug erl2 fn = do
    when debug $ putIOwords ["procTxt start", showT fn]
    -- erl2         <- readErlaubt fnErl
    -- erl  <- read6 fnErl txtFile -- reads lines
    -- let erlaubt = concat . map words' $ erl :: [Text]

    ls :: [Text] <- read8 fn textlinesFile

    let ls2 = map (procLine2 erl2) ls
    let res = False -- reports change - todo

    -- newfn <- changeExtensionBakOrNew debug fn
    writeWithBak debug fn textlinesFile ls2 
    return res  

    -- rest is copied
    -- if debug
    --     then do
    --     let fnnew = makeAbsFile (toFilePath fn <> "NEW")
    --     putIOwords ["procMd result in new", showT fnnew]
    --     write8 fnnew textlinesFile res
    --     putIOwords ["procMd result in new written", showT res, "fn", showT fnnew]
    --     else do
    --     let fnrename = fn <.> bakExtension :: Path Abs File
    --     renameOneFile (fn <.> txtExtension) fnrename
    --     putIOwords ["procMd renamed to bak", showT fnrename]
    -- write8 fn textlinesFile ls2
