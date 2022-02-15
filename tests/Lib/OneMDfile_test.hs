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
test_1 = assertEqual ("fuer\n") $ procMdTxt2 [] ("fuer\n"::Text)
-- test_2 = assertEqual 6 9
test_2 = assertEqual ("       fuer\n") $ procMdTxt2 [] ("       fuer\n"::Text)
test_3 = assertEqual ("\tfuer\n") $ procMdTxt2 [] ("\tfuer\n"::Text)
test_4 = assertEqual ("        Einschr\228nkungen gelebt.\n") $ procMdTxt2 [] ("        Einschränkungen gelebt.\n"::Text)
exampleText1 = unlines' [
        "abstract: Was hat einer in der Katastrophe erlebt, der weniger unter den Folgen zu"
        , "         leiden hatte, als andere? Wir haben meist ausserhalb von Wien am Land ohne wesentlichen"
        , "         Einschränkungen gelebt." ]
-- resultText1 = "abstract: Was hat einer in der Katastrophe erlebt, der weniger unter den Folgen zu\\n         leiden hatte, als andere? Wir haben meist ausserhalb von Wien am Land ohne wesentlichen\\n         Einschr\\228nkungen gelebt.\\n\""

test_5 = assertEqual exampleText1 $ procMdTxt2 [] exampleText1
-- test_6 = assertEqual resultText1 $ showT  exampleText1
test_7 = assertEqual ("        Einschr\228nkungen gelebt.\n    und\n") $ procMdTxt2 [] ("        Einschränkungen gelebt.\n    und"::Text)
-- test_5 = do -- fn1 muss existieren
--                 -- result fn1 renamed, new fn1 written 
--     r <- runErr $ 
--             do  -- necessary to keep the files for tests
--                 cont :: Text <- readFile2 fn1 
--                 fn2 <- changeExtensionBakOrNew True fn1 
--                 writeFile2 fn2 cont
--                 return (toFilePath fn2)
--     assertEqual (Right fn1t) r 
    
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
