-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


-- import System.Exit
-- import Uniform.FileIO
import UniformBase
-- import Lib.ProcPandocDatei

import           Test.Framework
import {-@ HTF_TESTS @-} Lib.ProcWord_test
import {-@ HTF_TESTS @-} Lib.ProcTxt_test
-- import {-@ HTF_TESTS @-} Lib.ProcPandocDatei_test
-- import {-@ HTF_TESTS @-} Lib.OneMDfile_test

main :: IO ()
main =  main1

main1 :: IO ()
main1 = do  -- with tests in other modules
    putStrLn "HTF ExampleTest.hs:\n"
    p <- htfMain htf_importedTests
    putStrLn ("HTF end StringConversion.hs test:\n"
         ++ show p)
    return ()

-- main2 :: IO ()
-- main2 = do
--  putStrLn "main2 start -----------"
--  let erlfn = makeAbsFile "/home/frank/Workspace8/replaceUmlaut/nichtUmlaute.txt"
--  let testfn = makeAbsFile "/home/frank/Workspace8/replaceUmlaut/testMarkdown0.md"
--  res <- runErr $  procMd2 True erlfn testfn
--  putStrLn (show res)
--  putStrLn "main2 end ............."

-- test_2 = assertEqual 6 9
