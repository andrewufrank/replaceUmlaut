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
-- import Uniform.Strings
import UniformBase
import Lib.ProcWord
import Control.Monad.Trans.Writer.Strict

openMain :: IO ()
openMain = do
    return ()



-- show produces the "xx"
test_1 = assertEqual ("für") $ procWord1 ("fuer"::Text)
-- test_2 = assertEqual 6 9

test_2 = assertEqual t2 $ map procWord1 t1
t1 = ["fuer", "Moerder", "Aerger", "koennen", "maesten", "FUER", "Oesterreich"] :: [Text]
t2 = ["für", "Mörder", "Ärger", "können", "mästen", "FÜR", "Österreich"] :: [Text]

test_e1 = assertEqual [True, True]  $ map checkErlaubt1 e1
e1 = ["Koeffizient", "Poetik"]::[Text]

test_e2 = assertEqual True $ isInfixOf' ("poes"::Text) "poesie"
test_w1 = assertEqual w1r $ map (procWord2 erlaubt1) w1
w1 = ["fuer", "Moerder", "Aerger", "koennen", "maesten", "FUER"
  , "Koeffizienten", "Poetik", "Poet", "Poesie"] :: [Text]
w1r = ["f\252r", "M\246rder", "\196rger", "k\246nnen", "m\228sten",
 "F\220R", "Koeffizienten", "Poetik", "Poet", "Poesie"]::[Text]

test_r1:: IO ()
test_r1 =  do
  r <- runErr $ do
  
        let t2 = execWriter $ procWord2Rep [] (unwords' t1) 
        putIOwords ["rest r1: t2",  t2]
        return ((""::Text ) /= t2)  -- null -> nothing changed
  assertEqual (Right True) r

test_r2:: IO ()
test_r2 =  do
  r <- runErr $ do
  
        let t2 = execWriter $ procLine2Rep [] (unwords' t1) 
        putIOwords ["rest r2: t2",  t2]
        return ((""::Text ) /= t2)  -- null -> nothing changed
  assertEqual (Right True) r
