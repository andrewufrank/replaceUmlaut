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
import           Lib.FileHandling     
-- import Lib.ProcPandocDatei
import MdDocHandling

-- for pandoc testing
import           Uniform.Pandoc

import qualified Text.Pandoc                   as P
import qualified Text.Pandoc.Walk              as PW
import qualified Data.Text   as T


-- newExtension :: Extension
-- newExtension = Extension "new" :: Extension
-- mdFile = makeTyped extMD
-- ^ filetype to read text in lines

-- procMd3 :: Bool -> [Text]-> Path Abs File -> ErrIO ()
-- procMd3 debug erl2 fn = do
-- --   erl2               <- readErlaubt fnErl
--   procMd1 debug erl2 fn 

-- --   ls :: MarkdownText <- read8 fn markdownFileType
--   putIOwords ["procMd3 done ",   "fn", showT fn]

--   ls2 <- unPandocM (changeUmlautInPandoc True erl2 (ls))

--   if debug
--     then do
--       let fnnew = makeAbsFile (toFilePath fn <> "NEW")
--       putIOwords ["procMd result in new", showT fnnew]
--       write8 fnnew markdownFileType ls2
--       putIOwords ["procMd result in new written", showT ls2, "fn", showT fnnew]
--     else do
--       let fnrename = fn <.> bakExtension :: Path Abs File
--       renameOneFile (fn <.> extMD) fnrename
--       putIOwords ["procMd renamed to bak", showT fnrename]


--   write8 fn markdownFileType ls2
--   when debug $ putIOwords ["procMd done", showT ls2]

procMd1 :: Bool -> [Text] -> Path Abs File -> ErrIO ()
-- ^ replace umlaut in a pandoc markdown file
-- unless it is an permitted group
-- in a file with extension txt
-- the original file is renamed to bak and the
-- corrected version written to the original filename
-- except when debug flag is set
-- then the new file is written to NEW
-- and the origianl file is not changed
-- debug true gives new file
procMd1 debug erl2 fn = do
    when debug $ putIOwords ["\n procMD1 ", showT fn, "file to process"]

    -- f0l :: LazyByteString <- readFile2 fn
    -- let f0 = bl2t f0l       -- why read lazyByteString?
    f0 <- read8 fn mdFile 
    let f1 =   mdDocRead . unlines' $ f0 :: MdDoc1   -- definition in MdDocHandling
    when debug $ putIOwords ["procMD1 yamlHeader", showT . yamlHeader1 $ f1]
    let german = mdocIsGerman f1  -- check requires pandoc?

    if german then  do 
            -- let f2 = updateMdDoc2 (procMdTxt erl2) (procMdTxt erl2) f1
            -- newfn <- changeExtensionBakOrNew debug fn  -- not debug?
            -- let f3 = mdDocWrite f2 
            let f3 =  map (procLine2 erl2)  f0 -- process the full file
            writeWithBak debug fn mdFile f3
            when True $ putIOwords ["\n procMd1 ", showT fn, "german file umlaut changed with backup"]
        else
            when debug $ putIOwords ["\n procMd1 ", showT fn, "not german file"]
           

    when debug $ putIOwords ["\n procMD1 ", showT fn, "file done with backup"]
    -- return ()


-- data Markdown = Markdown Text deriving (Show, Read, Eq, Ord)
-- ^ just for marking md files
-- mdFile :: TypedFile5 Text Markdown
-- mdExtension :: Extension
-- mdExtension = Extension "md" :: Extension



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

