-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


import System.Exit

import           Test.Framework
import {-@ HTF_TESTS @-} Lib.ProcWord_test

main =  main1

main1 = do  -- with tests in other modules
    putStrLn "HTF ExampleTest.hs:\n"
    p <- htfMain htf_importedTests
    putStrLn ("HTF end StringConversion.hs test:\n")
        --  ++ show p)
    return ()

-- main2 = do
--  putStrLn "main2 start -----------"
--  putStrLn "main2 end ............."

-- test_2 = assertEqual 6 9
