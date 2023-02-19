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
import Uniform.Pandoc (extMD)

procTxt :: Bool -> [Text] -> Path Abs File -> ErrIO ()
-- ^ replace umlaut unless it is an permitted group
-- in a file with extension txt (only!)
procTxt debug erl2 fn = do
    when debug $ putIOwords ["procTxt start", showT fn]
    let fnExtension = getExtension fn :: Extension
    
    -- erl2         <- readErlaubt fnErl
    -- erl  <- read6 fnErl txtFile -- reads lines
    -- let erlaubt = concat . map words' $ erl :: [Text]
    -- (fnext, fnbak, fnnew) <- changeExtensionBakOrNew debug fn

    if fnExtension == txtExtension 
        then do 
    --     then do
            ls :: [Text] <- read8 fn textlinesFile  -- split in lines
        --         return ls
        -- else do 
        --         ls  <- read8 fn mdFile
        --         return ls

            when debug $ putIOwords ["procTxt ls", showT ls]
            let ls2 = map (procLine2 erl2) ls
            when debug $ putIOwords ["procTxt ls2", showT ls2]
            let ls3 = unwrap7 ls2 :: Text
            when debug $ putIOwords ["procTxt unwrap7 . ls3", showT ls3]

            let res = ls2
            if debug 
                then write8 fn textlinesNewFile res
                else do 

                    renameToBak8 fn textlinesFile 
                    write8 fn textlinesFile res 
        else putIOwords ["ERROR: not txt file - nothing done!"]

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
