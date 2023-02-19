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

module Lib.ProcTextFile  -- (openMain, htf_thisModuelsTests)
                   where
-- import Uniform.Strings
-- import Uniform.TypedFile
-- import           Uniform.FileIO
-- import Uniform.Error
import           Lib.ProcWord
import UniformBase
import Lib.FileHandling
import Uniform.Pandoc (extMD)

procTextFile :: Bool -> [Text] -> Path Abs File -> ErrIO ()
-- ^ replace umlaut unless it is an permitted group
-- in a file with extension txt or md (only!)
procTextFile debug erl2 fn = do
    when debug $ putIOwords ["procTxt start", showT fn]
    let fnExtension = getExtension fn :: Extension
    
    let textLineType = if fnExtension == txtExtension 
        then textlinesFile 
        else if fnExtension == extMD 
            then mdFile 
            else errorT  ["ERROR: not txt file - nothing done!"]

    ls :: [Text] <- read8 fn textLineType  -- split in lines

    -- when debug $ putIOwords ["procTxt ls", showT ls]
    let ls2 = map (procLine2 erl2) ls
    -- when debug $ putIOwords ["procTxt ls2", showT ls2]
    let ls3 = unwrap7 ls2 :: Text
    -- when debug $ putIOwords ["procTxt unwrap7 . ls3", showT ls3]

    let res = ls2
    if debug 
        then write8 fn textlinesNewFile res
        else do 
            renameToBak8 fn textLineType 
            write8 fn textLineType res 

