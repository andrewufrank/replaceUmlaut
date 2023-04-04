-----------------------------------------------------------------------------
--
-- Module      : the main for calling replaceUmlaut functions
--    applied to specific files
--    with a switch for the txt and the filename
--    and recursively processing all md files 
--    but not dealing with the yml header language switch
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}


-- module ReplaceUmlaut where      -- must have Main (main) or Main where
module  Main (main)  where

-- import           Uniform.Convenience.StartApp
import Uniform.CmdLineArgs
import UniformBase
-- import           Data.Semigroup                 ( (<>) )
import           Lib.ProcTextFile
import           Lib.ProcDirMD
-- import           Lib.ProcPandocDatei
import              Lib.OneMDfile
import Lib.FileHandling

programName, progTitle :: Text
programName = "Replace umlaut in txt file " :: Text
progTitle =
  "replace ae, oe, and ue to umlaut (except when in nichtUmlaute.txt)" :: Text

-- to run add in .ghci -- tests/Testing.hs

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
      "the file (with extension .txt)"
    )
  return ()


--- cmd line parsing
data LitArgs = LitArgs 
    { isDebug :: Bool   -- ^ switch to debug mode (unchanged file, NEW extension)
    , isAllMD :: Bool   -- ^ collect all md files in directory
      , argfile  :: String -- ^ the filename absolute
      } deriving (Show)

cmdArgs :: Parser (LitArgs)
cmdArgs =
  LitArgs
    <$> switch 
            (long "debug" <> short 'd' <> help 
              "use debug mode; original file is unchange, new file with NEW extension attached")
    <*> switch 
            (long "all md" <> short 'm' <> help 
              "process all md files in directory")
    <*> argument str
                 (
      --   long "filename" <>
                  metavar "filename or directory")

parseAndExecute :: Text -> Text -> ErrIO ()
parseAndExecute t1 t2 = do
    args <- callIO $ execParser (opts2 cmdArgs t1 t2 )
    putIOwords ["parseAndExecute LitArgs", showT args]
    curr <- currentDir
    -- let dir0 = makeAbsDir "/home/frank/additionalSpace/DataBig/LitOriginals"
    let fn2     = argfile args :: FilePath
    -- let isText  = isTxt args :: Bool
    let debug   = isDebug args :: Bool
    let allMD   = isAllMD args :: Bool

    let fnErl =
            makeAbsFile "/home/frank/Workspace8/replaceUmlaut/nichtUmlaute.txt"
    erl2  <- readErlaubt fnErl
 
    if allMD 
        then do
            let dir = curr </> makeRelDir fn2 :: Path Abs Dir
            procDirMD debug erl2 dir
        else do
            let fn = curr </> makeRelFile fn2 :: Path Abs File
            procTextFile debug erl2 fn 

            -- let ext     = getExtension fn
            -- let isText2 = isText || ext == (Extension "txt") || ext == (Extension "md")
            -- if isText2 
            --     then 
                        
            --     else 
            --             procMd1 debug erl2 fn



    -- differences in md test for language and if nothing to do
        -- no new file is written
        
--  where
--   opts = info (helper <*> cmdArgs)
--               (fullDesc <> (progDesc . t2s $ t1) <> (header . t2s $ t2))



  --cmd2textstate :: LitArgs -> TextState2
  ---- fillst the textstate with directory and filename and proj
  ---- language is by default english, buchcode is the same as proj
  --cmd2textstate args  = fillTextState2 sourceTest4 generalityTest4
  --                 (argdir args) (argbuch args)
  --
