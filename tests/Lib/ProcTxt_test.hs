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


import Test.Framework
-- import Uniform.Strings
import Uniform.Error
import Lib.ProcTxt
import Lib.ProcWord (erlaubt1)
import Uniform.FileIO

openMain :: IO ()
openMain = do
    return ()



-- show produces the "xx"
test_1 = do
  r <- runErr $ do
            let fn = makeRelFile "testDaten" :: Path Rel File
            cdir <- currentDir
            let fnabs = cdir </> fn :: Path Abs File
            procTxt erlaubt1 fnabs
  assertEqual (Right ()) r
