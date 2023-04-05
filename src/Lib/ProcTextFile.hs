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
{-# LANGUAGE BlockArguments #-}

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
import Control.Monad.Trans.Writer.Strict

procTextFile :: Bool -> [Text] -> Path Abs File -> ErrIO Bool
-- ^ replace umlaut unless it is an permitted group
-- in a file with extension txt or md (only!)
-- returns True if something has changed
procTextFile debug erl2 fn = do
    when debug $ putIOwords ["procText start", showT fn]
    let fnExtension = getExtension fn :: Extension

    let textLineType
          | fnExtension == txtExtension = textlinesFile
          | fnExtension == extMD = mdFile
          | otherwise = errorT  ["ERROR: not text file - nothing done!"]

    ls :: [Text] <- read8 fn textLineType  -- split in lines

    -- when debug $ putIOwords ["procTxt ls", showT ls]
    -- let ls2 = map (procLine2 erl2) ls
    let (ls2,report) = runWriter $ mapM (procLine2Rep erl2) ls

    -- when debug $ 
    when debug $ putIOwords ["procText ls2", unlines' ls2]
    putIOwords ["procText report",  report]
    -- putIOwords ["procText file returned", unlines' ls]
    -- let ls3 = unwrap7 ls2 :: Text
    -- when debug $ putIOwords ["procTxt unwrap7 . ls3", showT ls3]

    let changed = zero /= report -- no report means nothing changed 
    when changed do 
        writeWithBak debug fn textLineType (  ls2)
        when True $ putIOwords ["procText changed file",  showNice fn, "rewritten"
            , "if some words in the report above are not correct"
            , "edit the file and add the form to 'doNotReplace'."]
    -- true - changed 
    when debug $ putIOwords ["procText done,   changed: ",  showT changed]

    return changed

    -- if debug 
    --     then write8 fn textlinesNewFile res
    --     else do 
    --         renameToBak8 fn textLineType 
    --         write8 fn textLineType res 

