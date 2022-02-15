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

module Lib.ProcPandocDatei (procMd) -- (openMain, htf_thisModuelsTests)
                           where
-- import Uniform.Strings
-- import Uniform.TypedFile
import           Uniform.FileIO
-- import Uniform.Error
import           Lib.ProcWord
import           Lib.ProcTxt                    ( bakExtension )

-- for pandoc testing
import           Uniform.Pandoc

import qualified Text.Pandoc                   as P
import qualified Text.Pandoc.Walk              as PW
-- import qualified Data.Text                     as T
-- import qualified Data.Text.IO                  as TIO

-- data Markdown = Markdown Text deriving (Show, Read, Eq, Ord)
-- ^ just for marking md files
-- mdFile :: TypedFile5 Text Markdown
-- mdExtension :: Extension
-- mdExtension = Extension "md" :: Extension
newExtension :: Extension
newExtension = Extension "new" :: Extension
-- mdFile = makeTyped extMD
-- ^ filetype to read text in lines


changeUmlautInPandoc
  :: Bool -> [Text] -> MarkdownText -> P.PandocIO MarkdownText
-- ^ changes the umlaut written as ae, oe und ue to umlaut
-- except if the words are included in the erlaubt list
changeUmlautInPandoc debug erlaubt dat = do  -- inside is the error handling
  when debug $ putIOwords ["changeUmlautInPandoc", take' 100 . showT $ dat]
      -- liftIO $ TIO.putStrLn . showT $ dat
  doc <- P.readMarkdown
    P.def
      { P.readerExtensions = P.extensionsFromList [P.Ext_yaml_metadata_block]
      }
    (unwrap7 dat) -- (T.pack "[testing](url)")
  when debug $ putIOwords ["changeUmlautInPandoc", take' 1000 . showT $ doc]
    -- liftIO $ TIO.putStrLn . showT $ doc
  -- here the processing
  let doc2 = umlautenStr erlaubt doc
  rst2 <- P.writeMarkdown P.def { P.writerSetextHeaders = False }
                            --   }
                            --   writerExtensions = strictExtensions,
                            -- , writerReferenceLinks = True -- use ref-style links
                          doc2
  when debug $ putIOwords ["changeUmlautInPandoc end", take' 100 . showT $ rst2]
  return . wrap7 $ rst2

-- pandocIOwrap :: (P.PandocIO MarkdownText) -> ErrIO MarkdownText
-- -- ^ just a wrapper for operatiosn in the PandocIO monad
-- -- handles the pandoc errors
-- -- Markdown is a text marked as Markdown
-- -- unPandocM ist in uniform
-- pandocIOwrap op = do
--   rst3 <- callIO $ do
--     result <- P.runIO $ op
--     rst1   <- P.handleError result
--     return rst1
--   -- TIO.putStrLn rst
--   return rst3

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
-- the original file is renamed to bak and the
-- corrected version written to the original filename
-- except when debug flag is set
-- then the new file is written to NEW
-- and the origianl file is not changed
procMd debug fnErl fn = do
  erl2               <- readErlaubt fnErl
  -- erl  <- read6 fnErl txtFile -- reads lines
  -- let erl2 = concat . map words' $ erl :: [Text]

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

  -- let ls2      = map (procLine erlaubt) ls

  write8 fn markdownFileType ls2
  when debug $ putIOwords ["procMd done", showT ls2]

procLine :: [Text] -> Text -> Text
procLine erlaubt ln = unwords' . map (procWord2 erlaubt) . words' $ ln
-- process all words in a line

-- instance TypedFiles7 Text Markdown where
--   wrap7 t = Markdown t
--   unwrap7 (Markdown t) = t
