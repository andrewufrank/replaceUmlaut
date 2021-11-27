{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | ... Module change all umlaut 
    processes all md files in the current directory
-}

module Main where

    -- https://www.vacationlabs.com/haskell/environment-setup.html 
    -- how to setup IDE 

    -- ghci support
    --https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode

-- import YamlRead  
import UniformBase
-- import qualified Data.Yaml as Y
-- import qualified Uniform.Yaml as Y
import Uniform.Json
import UniformBase
-- import Uniform.Convenience.StartApp

import Lib.ProcTxt
import Lib.ProcWord
import FileHandling
import MdDocHandling
import BlogDetails
-- import Uniform.StartApp

-- import System.Posix.Files as Posix 

-- fn1, fn2f :: FilePath
-- fn1 = "testData/test1.md"
-- fn2 = "testData/test2.md"
-- fn4 = "testData/test4.md"
-- fn2f = "testData/fail2.md"

    
main :: IO ()
main = startProg "replaceUmlaut" $ do 
    -- main4idempotent fn2  -- for testing
    -- main4defautlYAML
    -- testSingleFileSplit
    testAllMd
    
    return ()

testAllMd = do 
    let erlFn =
            makeAbsFile "/home/frank/Workspace11/replaceUmlaut/nichtUmlaute.txt"
    erl2         <- readErlaubt erlFn

    currDir :: Path Abs Dir <- currentDir
    let targetDir =  currDir 
    let testflag = False
    -- let targetDir =  addDir currDir (makeRelDir "testData copy")
    -- let testflag = True
    putIOwords ["\nProcessing all .md files in dir", showT targetDir]
    putIOwords ["\nFle with words not to convert:", showT erlFn]

    fs :: [Path Abs File] <- getDirContentFiles targetDir 
    let mds = filter (hasExtension mdExt ) fs

    -- fehlt noch filter fuer de_??

    putIOwords ["\nThe md files are:", unlines' . map showT $ mds]

    
    mapM (procMd testflag erl2) mds

    return () 

mdExt :: Extension
mdExt = Extension "md"


procMd :: p -> [Text] -> Path Abs File -> ErrorT Text IO ()
procMd debug erl2 fn = do 
    f0l :: LazyByteString <- readFile2 fn 
    let f0 = bl2t f0l

    let f1 = mdDocWrite 
                    -- . updateMdDoc id (procMdTxt erl2)
                    . procFileContentIfGerman erl2
                    . mdDocRead 
                    $ f0
                    :: Text 

    newfn <- changeExtensionBakOrNew False fn
    writeFile2 newfn f1
    putIOwords ["\n procMD ", showT fn, "file done"]
    return () 

procFileContentIfGerman :: [Text] ->  MdDoc1 -> MdDoc1
procFileContentIfGerman erl2 md1 = 
    if isGerman 
        then updateMdDoc id (procMdTxt erl2) md1
        else md1 
    where 
        h1 = yamlHeader1 md1 
        t1 = docText1 md1 
        mlang = getAtKey h1 "language" :: Maybe Text
        isGerman = case mlang of
                            Nothing -> False 
                            Just lang -> (lang ==  "de_AT" ||
                                            lang == "de_CH" ||
                                            lang == "de_DE")
        -- bh1 = fromJSON h1 :: BlogHeader 
        -- isGerman =  ("de_AT" ==) . language . bh1 :: Bool

procMdTxt :: [Text] ->  Text -> Text 
-- change all umlaut in text 
procMdTxt erl2 t = unlines' . map (procLine erl2) . lines' $ t 
