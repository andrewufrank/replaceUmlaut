-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

-- import           Lib.DirTree
-- import           Lib.OpenClass

main :: IO ()
main =  do  -- with tests in other modules
    -- dirMain
    -- openMain
    putStrLn "class main output"
    putStrLn "end"
    

