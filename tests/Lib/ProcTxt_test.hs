-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
--              tests the corresponding Lib.NN.hs program
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.ProcTxt_test  -- (openMain, htf_thisModuelsTests)
                        where


import           Test.Framework
-- import Uniform.Strings
-- import           Uniform.Error
import           Lib.ProcTxt
-- import           Lib.ProcWord                   ( erlaubt1 )
-- import           Uniform.FileIO
import UniformBase
import Lib.FileHandling

-- test_1 = do
--   r <- runErr $ do
--     let fn = makeRelFile "testDaten" :: Path Rel File
--     cdir <- currentDir
--     let fnabs = cdir </> fn :: Path Abs File
--     procTxt erlaubt1 fnabs
--   assertEqual (Right ()) r

test_2 :: IO ()
test_2 = do
  r <- runErr $ do
    let fn    = makeRelFile "testDaten" :: Path Rel File
    let fnerl = makeRelFile "nichtUmlaute" :: Path Rel File
    cdir <- currentDir
    let fnabs    = cdir </> fn :: Path Abs File
    let fnerlabs = cdir </> fnerl :: Path Abs File
    erl2 <- readErlaubt fnerlabs
    procTxt False erl2 fnabs
  assertEqual (Right ()) r

-- dictionary = "/home/frank/Workspace8/replaceUmlaut/de.dic"
testfn :: Path Abs File
testfn = makeAbsFile "/home/frank/Workspace11/replaceUmlaut/testcorona.txt"

test_3 :: IO ()
test_3 = do
  r <- runErr $ do
            -- let fn = makeRelFile "testDaten" :: Path Rel File
            -- let fnerl = makeRelFile "nichtUmlaute"::Path Rel File
    cdir <- currentDir
    -- let fnabs = cdir </> fn :: Path Abs File -- t
    let fnabs    = testfn :: Path Abs File
    let fnerl    = makeRelFile "nichtUmlaute" :: Path Rel File
    let fnerlabs = cdir </> fnerl :: Path Abs File
    erl2 <- readErlaubt fnerlabs
    procTxt False erl2 fnabs
  assertEqual (Right ()) r
