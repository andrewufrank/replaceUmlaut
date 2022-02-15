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

module Lib.OneMDfile_test  -- (openMain, htf_thisModuelsTests)
     where


import Test.Framework
-- import Uniform.Strings
-- import Uniform.Error 
import Lib.OneMDfile
-- import Lib.ProcWord
-- import Uniform.FileIO
import UniformBase

-- show produces the "xx"
test_1 = assertEqual ("für\n") $ procMdTxt2 [] ("fuer\n"::Text)
-- test_2 = assertEqual 6 9
test_2 = assertEqual ("       für\n") $ procMdTxt2 [] ("       fuer\n"::Text)
test_3 = assertEqual ("\tfür\n") $ procMdTxt2 [] ("\tfuer\n"::Text)
test_4 = assertEqual ("        Einschr\228nkungen gelebt.\n") $ procMdTxt2 [] ("        Einschränkungen gelebt.\n"::Text)
-- test_2 = assertEqual 6 9

-- test_2 = assertEqual t2 $ map procWord1 t1
-- t1 = ["fuer", "Moerder", "Aerger", "koennen", "maesten", "FUER", "Oesterreich"] :: [Text]
-- t2 = ["für", "Mörder", "Ärger", "können", "mästen", "FÜR", "Österreich"] :: [Text]

-- test_e1 = assertEqual [True, True]  $ map checkErlaubt1 e1
-- e1 = ["Koeffizient", "Poetik"]::[Text]

-- test_e2 = assertEqual True $ isInfixOf' ("poes"::Text) "poesie"
-- test_w1 = assertEqual w1r $ map (procWord2 erlaubt1) w1
-- w1 = ["fuer", "Moerder", "Aerger", "koennen", "maesten", "FUER"
--   , "Koeffizienten", "Poetik", "Poet", "Poesie"] :: [Text]
-- w1r = ["f\252r", "M\246rder", "\196rger", "k\246nnen", "m\228sten",
--  "F\220R", "Koeffizienten", "Poetik", "Poet", "Poesie"]::[Text]

-- w2 = unwords' w1 :: Text 
-- w2r = "f\252r M\246rder \196rger k\246nnen m\228sten F\220R Koeffizienten Poetik Poet Poesie"
-- test_idempot = assertEqual w2r (procLine erlaubt1 (procLine erlaubt1 w2))
