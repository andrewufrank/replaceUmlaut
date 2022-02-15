{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | ... Module change all umlaut in md files 
    processes all md files in the current directory
-}

-- module MdReplaceUmlaut where
module  Main (main)  where

import Uniform.CmdLineArgs
import UniformBase
import Uniform.Json
-- import UniformBase
-- import Uniform.Convenience.StartApp
import System.Directory.Recursive
-- import Options.Applicative
-- import Lib.ProcTxt
import Lib.ProcWord
import FileHandling
import MdDocHandling
import GHC.Generics (Generic1(to1))
-- import BlogDetails
import Data.Text (commonPrefixes)

programName, progTitle :: Text
programName = "Replace umlaut in md files in directory" :: Text
progTitle =
  "replace ae, oe, and ue to umlaut (except when in nichtUmlaute.txt)" :: Text


main :: IO ()
main = do
    startProg
        (unwords' [programName, progTitle])
        (parseAndExecute
            (unlinesT
                [ "converts in the file given"
                , "the umlaut written as ae, oe and ue"
                , "to regular umlaut. "
                , "execpt when in file nichtUmlaute"
                , "which is the list of words where ae, oe or ue must remain"
                ]
            )
            "the directory (absolute)"
        )
    return ()


--- cmd line parsing
data LitArgs = LitArgs { useTestData   :: Bool   -- ^ use test data
      , messageFlag :: Bool -- ^ produce test output
      , argdir  :: String -- ^ the dirname absolute
      } deriving (Show)

cmdArgs :: Parser (LitArgs)
cmdArgs =
  LitArgs
    <$> switch
          (long "test" <> short 't' <> help
            "test - uses md files in  ../ssg/docs/site/dough"
          )
    <*> switch
            (long "messages" <> short 'm' <> 
            help "include more output messages about processing"
            -- value False is default 
            )
    <*> strOption  
                 (
                    long "dirname" <>
                    short 'd' <>
                    metavar "directory (abs. path)" <>
                    value "currDir"
                )


parseAndExecute :: Text -> Text -> ErrIO ()
parseAndExecute t1 t2 = do
    args <- callIO $ execParser (opts2 cmdArgs t1 t2 )
    let erlFn =
            makeAbsFile "/home/frank/Workspace11/replaceUmlaut/nichtUmlaute.txt"
    erl2         <- readErlaubt erlFn

    let debug = messageFlag args

    currDir :: Path Abs Dir <- currentDir

    let dir2 = argdir args :: FilePath
    let dirTest = useTestData args

    putIOwords ["testAllMd", "1", showT dir2, showT dirTest]

    let targetDir
          | dirTest = makeAbsDir "/home/frank/Workspace11/ssg/docs/site/dough"
          | dir2 == "currDir" = currDir
          | otherwise = makeAbsDir dir2 :: Path Abs Dir
    when debug $ putIOwords ["testAllMd", "targetdir", showT targetDir]

    -- let targetDir =  addDir currDir (makeRelDir "testData copy")
    -- let testflag = True
    when debug $ putIOwords ["\nProcessing all .md files in german in dir", showT targetDir]
    when debug $ putIOwords ["\nFile with words not to convert:", showT erlFn]

    fns :: [FilePath] <- callIO $ getDirRecursive (toFilePath targetDir)
    when (debug) $ putIOwords ["testAllMd 1", "fns", showT . take 10 $ fns]

    let mds = filter (hasExtension "md") fns
    when ( debug) $ putIOwords ["testAllMd 2", "mds", showT . take 10 $ mds]

    let mds1 = filter (not . isInfixOf' "DNB") mds  -- settings file here not available
    when ( debug) $ putIOwords ["testAllMd 3", "mds1", showT . take 10 $ mds1]
    let mds2 = map makeAbsFile mds1
    when ( debug) $ putIOwords ["testAllMd 4", "mds2", showT . take 10 $ mds2]

    mapM_ (procMd1 debug erl2) mds2

    when ( debug) $ putIOwords ["testAllMd", "end"]
--   where
--     opts = info (helper <*> cmdArgs)
--               (fullDesc <> (progDesc . t2s $ t1) <> (header . t2s $ t2))


-- notDNB :: SiteLayout -> FilePath -> Bool 
-- notDNB siteLayout = not . isInfixOf' (t2s $ doNotPublish siteLayout)


-- mdExt :: Extension
-- mdExt = Extension "md"


procMd1 :: Bool -> [Text] -> Path Abs File -> ErrIO ()
procMd1 debug erl2 fn = do
    f0l :: LazyByteString <- readFile2 fn
    let f0 = bl2t f0l
    when debug $ putIOwords ["\n procMD ", showT fn, "file to process"]

    let f1 = -- mdDocWrite
                    -- . updateMdDoc id (procMdTxt erl2)
                    -- . procFileContentIfGerman debug erl2
                    -- . 
                    mdDocRead f0
                    :: MdDoc1
    let german = mdocIsGerman f1
    when german $  do 
            -- let f2 = updateMdDoc2 (procMdTxt erl2) (procMdTxt erl2) f1
            newfn <- changeExtensionBakOrNew False fn  -- not debug?
            -- let f3 = mdDocWrite f2 
            let f3 = procMdTxt erl2 f0 -- process the header with it
            writeFile2 newfn f3
            when True $ putIOwords ["\n procMD1 ", showT fn, "german file umlaut changed with backup"]
            

    when debug $ putIOwords ["\n procMD ", showT fn, "file done with backup"]
    -- return ()

-- procFileContentIfGerman :: Bool -> [Text] ->  MdDoc1 -> MdDoc1
-- procFileContentIfGerman debug erl2 md1 =
--     if isGerman
--         then updateMdDoc id (procMdTxt erl2) md1
--         else md1
--     where

-- mdocIsGerman :: MdDoc1 -> Bool 
-- mdocIsGerman md1 = isGerman 
--     where
--         h1 = yamlHeader1 md1
--         -- t1 = docText1 md1
--         mlang = getAtKey h1 "language" :: Maybe Text
--         -- isGerman = case mlang of
--         --                     Nothing -> False
--         --                     Just lang -> (lang ==  "de_AT" ||
--         --                                     lang == "de_CH" ||
--         --                                     lang == "de_DE")
    
--         isGerman = case mlang of
--                                 Nothing -> False
--                                 Just lang -> "de" `isPrefixOf'` lang
    

