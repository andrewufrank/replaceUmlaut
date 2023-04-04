-----------------------------------------------------------------------------
--
-- Module      : the main for calling replaceUmlaut functions
--    with a switch for the txt and the filename
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}


module ReplaceUmlaut  where      -- must have Main (main) or Main where

import UniformBase
-- import           Uniform.Convenience.StartApp
import           Uniform.Filenames           --   ( makeExtension )
-- import           Data.Semigroup                 ( (<>) )
import           Options.Applicative.Builder
import           Options.Applicative
import           Lib.ProcTxt
import Lib.OneMDfile
-- import           Lib.ProcPandocDatei
import Lib.FileHandling

programName, progTitle :: Text
programName = "Umlaut in md or txt file " :: Text
progTitle =
  "replace ae, oe, and ue to umlaut (except when in nichtUmlaute.txt)" :: Text

-- to run add in .ghci -- tests/Testing.hs

main :: IO ()
main = do
  startProg
    programName
    -- progTitle
    (parseAndExecute
      (unlinesT
        [ "converts words in the file given where"
        , "the umlaut is written as ae, oe and ue"
        , "to regular umlaut, "
        , "execpt when in file nichtUmlaute"
        , "which is the list of words where ae, oe or ue must remain."
        ]
      )
      "the file (with extension .txt or .md)"
    )
  return ()


--- cmd line parsing
data LitArgs = LitArgs { isTxt   :: Bool   -- ^ is this a txt file
      , argfile  :: String -- ^ the filename absolute
      } deriving (Show)

cmdArgs :: Parser (LitArgs)
cmdArgs =
  LitArgs
    <$> switch
          (long "txt" <> short 't' <> help
            "true if this is a txt file, txt or md extension is recognized"
          )
    <*> argument str
                 (
      --   long "filename" <>
                  metavar "filename")

parseAndExecute :: Text -> Text -> ErrIO ()
parseAndExecute t1 t2 = do
        args <- callIO $ execParser opts
        putIOwords ["parseAndExecute LitArgs", showT args]
        curr <- currentDir
        -- let dir0 = makeAbsDir "/home/frank/additionalSpace/DataBig/LitOriginals"
        let fn2     = argfile args :: FilePath
        let fn = curr </> makeRelFile fn2 :: Path Abs File
        let isText  = isTxt args :: Bool

        let ext     = getExtension fn
        let isText2 = isText || ext == (Extension "txt")
        let debug   = False
        let erlFn = makeAbsFile "/home/frank/Workspace8/replaceUmlaut/nichtUmlaute.txt"
        erl2               <- readErlaubt erlFn
        if isText2 then procTxt debug erl2 fn else procMd1 debug erl2 fn
    where
        opts = info (helper <*> cmdArgs)
              (fullDesc <> (progDesc . t2s $ t1) <> (header . t2s $ t2))


   