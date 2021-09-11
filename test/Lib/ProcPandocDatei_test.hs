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

module Lib.ProcPandocDatei_test  -- (openMain, htf_thisModuelsTests)
                                where


import           Test.Framework
-- import Uniform.Strings
import           Uniform.Error
import           Lib.ProcPandocDatei
-- import           Lib.ProcWord                   ( erlaubt1 )
import           Uniform.FileIO
-- import Uniform.TypedFile
-- import Lib.FileHandling (newExtension)
-- import Uniform.Pandoc (  markdownFileType, MarkdownText)

test_p2 :: IO ()  -- braucht testMarkdown0 existiert
test_p2 = do
  r <- runErr $ do
    let fn    = makeRelFile "testMarkdown1" :: Path Rel File
    let fnerl = makeRelFile "nichtUmlaute" :: Path Rel File
    cdir <- currentDir
    let fnabs    = cdir </> fn :: Path Abs File
    let fnerlabs = cdir </> fnerl :: Path Abs File
    putIOwords ["test_p2 fnabs", showT fnabs]
   
    procMd True fnerlabs fnabs
    let fnold =  fnabs 
    let fnnew =  makeAbsFile (toFilePath fnabs <> "NEW")
    oldcont :: MarkdownText <- read8 fnold markdownFileType
    newcont :: MarkdownText <- read8 fnnew markdownFileType
    putIOwords ["test_p2 oldcont", showT fnold, "is", showT oldcont]
    putIOwords ["test_p2 newcont", showT fnnew, "is", showT newcont]
    return (oldcont == newcont) 
  assertEqual (Right True)  r 

-- dictionary = "/home/frank/Workspace8/replaceUmlaut/de.dic"
testfn :: Path Abs File
testfn = makeAbsFile "/home/frank/Workspace8/replaceUmlaut/testcorona.txt"

-- test_3 :: IO ()
-- test_3 = do
--   r <- runErr $ do
--             -- let fn = makeRelFile "testDaten" :: Path Rel File
--             -- let fnerl = makeRelFile "nichtUmlaute"::Path Rel File
--     cdir <- currentDir
--     -- let fnabs = cdir </> fn :: Path Abs File -- t
--     let fnabs    = testfn :: Path Abs File
--     let fnerl    = makeRelFile "nichtUmlaute" :: Path Rel File
--     let fnerlabs = cdir </> fnerl :: Path Abs File
--     procTxt fnerlabs fnabs
--   assertEqual (Right ()) r
