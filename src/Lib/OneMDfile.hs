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

module Lib.OneMDfile  -- (openMain, htf_thisModuelsTests)
                           where
import UniformBase
import           Lib.ProcWord
import           Lib.FileHandling     ( bakExtension )
import Lib.ProcPandocDatei

-- for pandoc testing
import           Uniform.Pandoc

import qualified Text.Pandoc                   as P
import qualified Text.Pandoc.Walk              as PW
import qualified Data.Text   as T

-- data Markdown = Markdown Text deriving (Show, Read, Eq, Ord)
-- ^ just for marking md files
-- mdFile :: TypedFile5 Text Markdown
-- mdExtension :: Extension
-- mdExtension = Extension "md" :: Extension

newExtension :: Extension
newExtension = Extension "new" :: Extension
-- mdFile = makeTyped extMD
-- ^ filetype to read text in lines

procMd2 :: Bool -> Path Abs File -> Path Abs File -> ErrIO ()
-- ^ replace umlaut in a pandoc markdown file
-- unless it is an permitted group
-- in a file with extension txt
-- the original file is renamed to bak and the
-- corrected version written to the original filename
-- except when debug flag is set
-- then the new file is written to NEW
-- and the origianl file is not changed
procMd2 debug fnErl fn = do
  erl2               <- readErlaubt fnErl
  ls :: MarkdownText <- read8 fn markdownFileType
  putIOwords ["procMd ls", showT ls, "fn", showT fn]

  ls2 <- unPandocM (changeUmlautInPandoc True erl2 (ls))

  if debug
    then do
      let fnnew = makeAbsFile (toFilePath fn <> "NEW")
      putIOwords ["procMd result in new", showT fnnew]
      write8 fnnew markdownFileType ls2
      putIOwords ["procMd result in new written", showT ls2, "fn", showT fnnew]
    else do
      let fnrename = fn <.> bakExtension :: Path Abs File
      renameOneFile (fn <.> extMD) fnrename
      putIOwords ["procMd renamed to bak", showT fnrename]


  write8 fn markdownFileType ls2
  when debug $ putIOwords ["procMd done", showT ls2]

procMdTxt2 :: [Text] ->  Text -> Text
-- change all umlaut in text 
-- preserve leading blanks of line, or tabs, but not mixture of these
procMdTxt2 erl2  = unlines' . map (procLine2 erl2) . lines' 

procLine2 :: [Text] ->  Text -> Text
-- process one line preserving spaces or tabs (but not a mix) at start
procLine2 erl2 t = concat' [ld,t1]
    where
        (ld, t1) = case mb1 t of
                Nothing -> case mb2 t of 
                                Nothing -> ("", t)
                                Just (lead2, _, t02) -> (lead2,t02)
                Just (lead, _, t0) ->  (lead,t0)
        mb1 tx = T.commonPrefixes "                  " tx
        mb2 ty = T.commonPrefixes "\t\t\t\t\t\t\t" ty


-- procMdTxt2 :: [Text] ->  Text -> Text
-- -- change all umlaut in text 
-- -- preserve leading blanks of line, or tabs, but not mixture of these
-- procMdTxt2 erl2 t = concat' [ld,t2]
--     where
--         t2 = unlines' . map (procLine erl2) . lines' $ t1 
--         (ld, t1) = case mb1 t of
--                 Nothing -> case mb2 t of 
--                                 Nothing -> ("", t)
--                                 Just (lead2, _, t02) -> (lead2,t02)
--                 Just (lead, _, t0) ->  (lead,t0)
--         mb1 tx = T.commonPrefixes "                  " tx
--         mb2 ty = T.commonPrefixes "\t\t\t\t\t\t\t" ty

