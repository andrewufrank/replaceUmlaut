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

module Lib.ProcDirMD -- (openMain, htf_thisModuelsTests)
                   where
-- import Uniform.Strings
-- import Uniform.TypedFile
-- import           Uniform.FileIO
-- import Uniform.Error
import           Lib.ProcTextFile
import Lib.ProcWord
import UniformBase
import Lib.FileHandling

procDirMD :: Bool -> [Text] -> Path Abs Dir -> ErrIO ()
-- ^ replace umlaut in all md files in directory (recursively!)
procDirMD debug erl2 dir = do  
    when debug $ putIOwords ["procDirMD start", showT dir]
    -- pipedDoIOwithFilter :: Path Abs File -> Path Abs Dir -> Extension -> (Path Abs File -> ErrIO String) -> ErrIO ()
    curr <- currentDir
    let msgFile = curr </> makeRelFile "msg4procDir.txt"
    pipedDoIOwithFilter msgFile dir (Extension "md") (procTxt0 debug erl2)

    msg2 <- read8 msgFile textlinesFile
    putIOwords ["processing for ", showT dir, "done with msg", showT msg2]
    return ()

procTxt0 :: Bool -> [Text] -> Path Abs File -> ErrIO String 
procTxt0 debug erl2 fn = do 
    when debug $ putIOwords ["procTxt0 start", showT fn]
    procTextFile debug erl2 fn 
    when debug $ putIOwords ["procTxt0 end", showT fn]
    return . show $ fn