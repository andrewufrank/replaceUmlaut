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

module Lib.ProcWord_test  -- (openMain, htf_thisModuelsTests)
     where


import Test.Framework
import Uniform.Strings
import Lib.ProcWord

openMain :: IO ()
openMain = do
    return ()



-- show produces the "xx"
test_1 = assertEqual ("für") $ procWord1 ("fuer"::Text)
-- test_2 = assertEqual 6 9

test_2 = assertEqual t2 $ map procWord1 t1
t1 = ["fuer", "Moerder", "Aerger", "koennen", "maesten", "FUER"] :: [Text]
t2 = ["für", "Mörder", "Ärger", "können", "mästen", "FÜR"] :: [Text]

test_e1 = assertEqual [True, True]  $ map checkErlaubt1 e1
e1 = ["Koeffizient", "Poetik"]::[Text]

test_e2 = assertEqual True $ isInfixOf' ("poes"::Text) "poesie"
