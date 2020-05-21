-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
--              tests the corresponding Lib.NN.hs program
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
-- {-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.ProcWord_test  -- (openMain, htf_thisModuelsTests)
     where


-- import Test.Framework
-- import Uniform.Strings

openMain :: IO ()
openMain = do
    return ()



-- show produces the "xx"
test_1 = assertEqual ("für") $ procWord "fuer"
-- test_2 = assertEqual 6 9
