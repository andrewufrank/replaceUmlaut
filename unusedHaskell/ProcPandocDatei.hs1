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
-- import           Uniform.FileIO
-- import Uniform.Error
import UniformBase
import           Lib.ProcWord
import           Lib.FileHandling     ( bakExtension )

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
  let t7 = unwrap7 dat :: Text
  doc :: Pandoc <- P.readMarkdown
    P.def
      { P.readerExtensions = P.extensionsFromList [P.Ext_yaml_metadata_block]
      } t7
       -- (T.pack "[testing](url)")
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



umlautenStr :: [Text] -> P.Pandoc -> P.Pandoc
umlautenStr erlaubt = PW.walk umlauten
 where
  umlauten :: P.Inline -> P.Inline
  umlauten (P.Str w) = P.Str ( procWord2 erlaubt w)
  umlauten x         = x

-- use callPandoc
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
 
