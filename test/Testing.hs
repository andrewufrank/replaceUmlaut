 
-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main     where      -- must have Main (main) or Main where


-- import System.Exit
import Uniform.FileIO
import Lib.ProcPandocDatei

import           Test.Framework
-- import {-@ HTF_TESTS @-} Lib.FileHandling_test
-- import {-@ HTF_TESTS @-} Lib.ProcWord_test
-- import {-@ HTF_TESTS @-} Lib.ProcTxt_test
import {-@ HTF_TESTS @-} Lib.ProcPandocDatei_test

main :: IO ()
main =  main2

main1 :: IO ()
main1 = do  -- with tests in other modules
    putStrLn "HTF ExampleTest.hs:\n"
    p <- htfMain htf_importedTests
    putStrLn ("HTF end StringConversion.hs test:\n"
         ++ show p)
    return ()
    
erlfn = makeAbsFile "/home/frank/Workspace8/replaceUmlaut/nichtUmlaute.txt" :: Path Abs File
    
main2 :: IO ()
main2 = do
    putIOwordsT ["main2 start -----------"::Text]
    -- let testfn1 = makeAbsFile "/home/frank/Workspace8/replaceUmlaut/testMarkdown0.md":: Path Abs File
    let testfn1 = makeAbsFile "/home/frank/Workspace8/replaceUmlaut/test4.md":: Path Abs File
    res <- runErr $  procMd True erlfn testfn1
    putIOwords ["res after 1", showT res] 
    datfn1 <- readFile . toFilePath $ testfn1 
    putIOwords ["testfn1 ", showT . s2t $ datfn1 ]
    putIOwords ["\n test idempotent------------\n"::Text]
    let testfn2 = makeAbsFile "/home/frank/Workspace8/replaceUmlaut/test4NEW.md":: Path Abs File
    res2 <- runErr $ procMd True erlfn testfn2
    putIOwords [showT res]
    datfn2 <- readFile . toFilePath $ testfn2 
    putIOwords ["testfn1 ", showT . s2t $ datfn1 ]
    putIOwords ["testfn2 ", showT . s2t $ datfn2 ]
    putIOwords ["main2 end ............. is idempotent"::Text, showT (datfn1 == datfn2)]

main3 :: IO ()
main3 = do 
    res <- runErr $ 
        do 
            putIOwords ["main3 start ---------------------------------"]
            let fn80 = makeAbsFile "/home/frank/Workspace8/replaceUmlaut/test80.md"
            procMd True erlfn fn80
            return ()
    putIOwords ["main3 end", showT res]
