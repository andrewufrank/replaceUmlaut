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
import Lib.ProcTxt
import Lib.ProcWord
import Lib.FileHandling
-- import Uniform.FileIO
import UniformBase

-- show produces the "xx"
test_a1 = assertEqual ("a") $ unwords' . words' $ ("a"::Text) 
test_1 = assertEqual ("für") $ procLine2 [] ("fuer"::Text)
-- not clear where the \n disappeared 
-- test_2 = assertEqual 6 9
test_2 = assertEqual ("       für") $ procLine2 [] ("       fuer"::Text)
test_3 = assertEqual ("\tfür") $ procLine2 [] ("\tfuer"::Text)
test_4 = assertEqual ("        Einschr\228nkungen gelebt.") $ procLine2 [] ("        Einschraenkungen gelebt."::Text)


exampleText1 = unlines' [
        "abstract: Was hat einer in der Katastrophe erlebt, der weniger unter den Folgen zu"
        , "         leiden hatte, als andere? Wir haben meist ausserhalb von Wien am Land ohne wesentlichen"
        , "     Einschraenkungen gelebt." ] :: Text
resultText1 = unlines' [
        "abstract: Was hat einer in der Katastrophe erlebt, der weniger unter den Folgen zu"
        , "         leiden hatte, als andere? Wir haben meist ausserhalb von Wien am Land ohne wesentlichen"
        , "     Einschränkungen gelebt." ] :: Text

procLines1 :: [Text] -> Text -> Text
procLines1 e t = unlines' . fmap   (procLine2 e) . lines' $ t
test_5 = assertEqual resultText1 $ procLines1 [] exampleText1
-- test_6 = assertEqual "" $ showT  exampleText1
test_7 = assertEqual ("        Einschr\228nkungen gelebt.\n    und\n") $ procLines1 [] ("        Einschraenkungen gelebt.\n    und"::Text)


test_m1 = do
    r <- runErr $ do
        let fn1 = makeRelFile "testData/test4.md"
            fnerl = makeRelFile "nichtUmlaute" :: Path Rel File
        cdir <- currentDir
        let fnabs    = cdir </> fn1 :: Path Abs File
        let fnerlabs = cdir </> fnerl :: Path Abs File
        erl2 <- readErlaubt fnerlabs
        procMd1 True erl2 fnabs
    assertEqual (Right False) r

-- test a simple md file 
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
