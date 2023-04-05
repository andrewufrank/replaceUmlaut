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

module Lib.ProcText_test  -- (openMain, htf_thisModuelsTests)
                        where


import           Test.Framework
-- import Uniform.Strings
-- import           Uniform.Error
import           Lib.ProcTxt
-- import           Lib.ProcWord                   ( erlaubt1 )
-- import           Uniform.FileIO
import UniformBase
import Lib.FileHandling
import Lib.ProcTextFile
-- test_1 = do
--   r <- runErr $ do
--     let fn = makeRelFile "testDaten" :: Path Rel File
--     cdir <- currentDir
--     let fnabs = cdir </> fn :: Path Abs File
--     procTxt erlaubt1 fnabs
--   assertEqual (Right ()) r

 
-- dictionary = "/home/frank/Workspace8/replaceUmlaut/de.dic"
testfn :: Path Abs File
testfn = makeAbsFile "/home/frank/Workspace11/replaceUmlaut/testData/corona.txt"

 

 
test_ohneErlaubt :: IO ()
-- ohne erlaubte
test_ohneErlaubt = do
    r <- runErr $ do
        let fn1 = makeRelFile "testData/test4.md"
        cdir <- currentDir
        let fnabs    = cdir </> fn1 :: Path Abs File
        procTextFile True [] fnabs
    assertEqual (Right True) r

test_mitErlaubt :: IO ()
-- mit erluabten 
test_mitErlaubt = do
    r <- runErr $ do
        let fn1 = makeRelFile "testData/test4.md"
            fnerl = makeRelFile "nichtUmlaute" :: Path Rel File
        cdir <- currentDir
        let fnabs    = cdir </> fn1 :: Path Abs File
        let fnerlabs = cdir </> fnerl :: Path Abs File
        erl2 <- readErlaubt fnerlabs
        procTextFile True erl2 fnabs
    assertEqual (Right True) r

