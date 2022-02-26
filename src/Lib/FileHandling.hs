-----------------------------------------------------------------------------
-- Module      :   Convert umlaut written as ae, oe or ue into ä, ö and ü
--              in a single word
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
                -- -fno-warn-missing-signatures
            -- -fno-warn-missing-methods 
            
module Lib.FileHandling  -- (openMain, htf_thisModuelsTests)
                    where
import UniformBase
-- import           Uniform.Strings
-- import           Uniform.FileIO
import Uniform.Pandoc (extMD)

textlinesFile :: TypedFile5 Text [Text]
-- txtFile = makeTyped txtExtension
textlinesFile = makeTyped txtExtension
txtExtension :: Extension
txtExtension = Extension "txt" :: Extension
newExtension :: Extension
newExtension = Extension "new" :: Extension
bakExtension :: Extension
bakExtension = Extension "bak" :: Extension
-- ^ filetype to read text in lines

changeExtensionBakOrNew :: Bool -> Path Abs File -> ErrIO (Path Abs File)
-- ^ the given file is renamed as bak and the original filename returned
-- or (in casse of debug) the filename is changed with addition of 
-- NEW appended to filename (not extension)
changeExtensionBakOrNew debug fn = if debug
      then do
          putIOwords ["changeExtensionBakOrNew  fn", showT fn]
          let fnnew =   makeAbsFile ((removeExtension $ toFilePath fn) <> "NEW")
          putIOwords ["changeExtensionBakOrNew fnnew", showT fnnew]
          -- write8 fnnew markdownFileType ls4
          -- putIOwords ["procMd result in new written", showT ls4, "fn", showT fnnew]
          return fnnew
      else do
          let fnrename = fn <.> bakExtension :: Path Abs File
          renameOneFile (fn <.> extMD) fnrename
          putIOwords ["changeExtensionBakOrNew", 
                        "procMd renamed to bak", showT fnrename]
          return fn

instance TypedFiles7 Text [Text] where
  wrap7 t = words' t
  unwrap7 t = unwords' t

readErlaubt :: Path Abs File -> ErrIO [Text]
-- read the erlaubte words wtih ae, oe and ue
readErlaubt fnErl = do
  erl :: [Text] <- read8 fnErl textlinesFile -- reads lines
  let erl2 = erl -- concat . map words' $ erl :: [Text]
  return erl2