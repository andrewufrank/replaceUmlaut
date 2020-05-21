-----------------------------------------------------------------------------
-- Module      :   Convert umlaut written as ae, oe or ue into ä, ö and ü
--              in a txt file
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.ProcPandocDatei  -- (openMain, htf_thisModuelsTests)
                           where
-- import Uniform.Strings
-- import Uniform.TypedFile
import           Uniform.FileIO
-- import Uniform.Error
import           Lib.ProcWord
import           Lib.ProcTxt                    ( txtFile )

-- for pandoc testing
import qualified Text.Pandoc                   as P
import qualified Text.Pandoc.Walk              as PW
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

data Markdown = Markdown Text deriving (Show, Read, Eq, Ord)
-- ^ just for marking md files
mdFile :: TypedFile5 Text Markdown
mdExtension = Extension "md" :: Extension
bakExtension = Extension "bak" :: Extension
newExtension = Extension "new" :: Extension
mdFile = makeTyped mdExtension
-- ^ filetype to read text in lines

readPandoc :: [Text] -> Markdown -> IO Markdown
readPandoc erlaubt dat = do
  result <- P.runIO $ do
    liftIO $ TIO.putStrLn . showT $ dat
    doc <- P.readMarkdown P.def (unwrap7 dat) -- (T.pack "[testing](url)")
    liftIO $ TIO.putStrLn . showT $ doc
    -- here the processing
    let doc2 = umlautenStr erlaubt doc
    P.writeMarkdown P.def doc2
  rst <- P.handleError result
  TIO.putStrLn rst
  return . wrap7 $ rst

umlautenStr :: [Text] -> P.Pandoc -> P.Pandoc
umlautenStr erlaubt = PW.walk umlauten
 where
  umlauten :: P.Inline -> P.Inline
  umlauten (P.Str w) = P.Str (t2s . procWord2 erlaubt . s2t $ w)
  umlauten x         = x

procMd :: Bool -> Path Abs File -> Path Abs File -> ErrIO ()
-- ^ replace umlaut in a pandoc markdown file
-- unless it is an permitted group
-- in a file with extension txt
procMd debug fnErl fn = do
  erl <- read6 fnErl txtFile -- reads lines
  let erlaubt = concat . map words' $ erl :: [Text]

  ls :: Markdown <- read8 fn mdFile
  putIOwords ["procMd ls", showT ls, "fn", showT fn]
  res <- liftIO $ readPandoc erl ls

  if debug
    then do
      let fnnew = makeAbsFile (toFilePath fn <> "NEW")
      putIOwords ["procMd result in new", showT fnnew]
      write8 fnnew mdFile res
      putIOwords ["procMd result in new written", showT res, "fn", showT fnnew]
    else do
      let fnrename = fn <.> bakExtension :: Path Abs File
      renameOneFile (fn <.> mdExtension) fnrename
      putIOwords ["procMd renamed to bak", showT fnrename]

  -- let ls2      = map (procLine erlaubt) ls

  -- write6 fn txtFile ls2
  putIOwords ["procMd done", showT res]

procLine :: [Text] -> Text -> Text
procLine erlaubt ln = unwords' . map (procWord2 erlaubt) . words' $ ln
-- process all words in a line

instance TypedFiles7 Text Markdown where
  wrap7 t = Markdown t
  unwrap7 (Markdown t) = t
