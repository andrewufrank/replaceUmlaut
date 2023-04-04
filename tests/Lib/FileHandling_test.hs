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

module Lib.FileHandling_test  -- (openMain, htf_thisModuelsTests)
     where


import Test.Framework
-- import Uniform.Strings
-- import Uniform.Error 
import Lib.FileHandling
-- import Lib.ProcWord
import Uniform.FileIO
import UniformBase

openMain :: IO ()
openMain = do
    return ()
-- tests to only check that filenames are correct
-- not that correct content

fn1 = makeAbsFile 
        "/home/frank/Workspace8/replaceUmlaut/testMarkdown1.md"
fn1t = makeAbsFile "/home/frank/Workspace8/replaceUmlaut/testMarkdown1.mdNEW"
fn1bak = makeAbsFile "/home/frank/Workspace8/replaceUmlaut/testMarkdown1.bak"
-- show produces the "xx"
test_1 = do -- fn1 muss existieren
                -- result fn1 renamed, new fn1 written 
    r <- runErr $ 
            do  -- necessary to keep the files for tests
                cont :: [Text] <- read8 fn1 mdFile
                writeWithBak True fn1 mdFile cont
                doesFileExist' fn1t
                -- return ()
    assertEqual (Right True) r 

test_2 = do   -- filename does not change but bak is written 
    r <- runErr $ 
            do 
                cont :: [Text] <- read8 fn1 mdFile
                writeWithBak False fn1 mdFile cont
                doesFileExist' fn1bak
    assertEqual (Right True) r 

