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
import Uniform.Error 
import Lib.FileHandling
-- import Lib.ProcWord
import Uniform.FileIO

openMain :: IO ()
openMain = do
    return ()
-- tests to only check that filenames are correct
-- not that correct content

fn1 = makeAbsFile 
        "/home/frank/Workspace8/replaceUmlaut/testMarkdown1.md"
fn1t = "/home/frank/Workspace8/replaceUmlaut/testMarkdown1.mdNEW"
-- show produces the "xx"
test_1 = do -- fn1 muss existieren
                -- result fn1 renamed, new fn1 written 
    r <- runErr $ 
            do  -- necessary to keep the files for tests
                cont :: Text <- readFile2 fn1 
                fn2 <- changeExtensionBakOrNew True fn1 
                writeFile2 fn2 cont
                return (toFilePath fn2)
    assertEqual (Right fn1t) r 

test_2 = do   -- filename does not change but bak is written 
    r <- runErr $ 
            do 
                cont :: Text <- readFile2 fn1 
                fn2 <- changeExtensionBakOrNew False fn1 
                writeFile2 fn2 cont 
                return (toFilePath fn2)
    assertEqual (Right . toFilePath $ fn1) r 

