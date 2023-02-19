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
textlinesNewFile :: TypedFile5 Text [Text]
textlinesNewFile = makeTyped newExtension 
textlinesBakFile :: TypedFile5 Text [Text]
textlinesBakFile = makeTyped bakExtension 

mdFile :: TypedFile5 Text [Text]
mdFile = makeTyped extMD

txtExtension :: Extension
txtExtension = Extension "txt" :: Extension
newExtension :: Extension
newExtension = Extension "new" :: Extension
bakExtension :: Extension
bakExtension = Extension "bak" :: Extension
-- ^ filetype to read text in lines

-- changeExtensionBakOrNew :: Bool -> Path Abs File -> ErrIO (Extension, Path Abs File, Path Abs File)
-- -- ^ the given fn is split in extension and 
-- -- returns the bak and the new file name  
-- changeExtensionBakOrNew debug fn = do
--     let fnNaked = makeAbsFile $ getParentDir fn </> getNakedFileName fn :: Path Abs File
--     let fnExtension = getExtension fn :: Extension
--     when debug $ putIOwords ["changeExtensionBakOrNew fnNaked", showT fnNaked, "fnExtension", showT fnExtension]
 
--     let fnbak = fnNaked <.> bakExtension :: Path Abs File
--     let fnnew = fnNaked <.> newExtension :: Path Abs File

--     putIOwords ["changeExtensionBakOrNew fnbak", showT fnbak]
--     putIOwords ["changeExtensionBakOrNew fnnew", showT fnnew]
    
--     return (fnExtension, fnbak, fnnew)

instance TypedFiles7 Text [Text] where  -- creates sequence of lines
  wrap7 t = lines' t  
  unwrap7 t = unlines' t 

readErlaubt :: Path Abs File -> ErrIO [Text]
-- read the erlaubte words wtih ae, oe and ue
readErlaubt fnErl = do
  erl :: [Text] <- read8 fnErl textlinesFile -- reads lines
  let erl2 =  concat . map words' $ erl :: [Text]
  return erl2

writeWithBak :: Bool -> Path Abs File -> TypedFile5 Text [Text]-> [Text] -> ErrIO () 
-- ^ write the text into a file; use path given after renaming the file to bak
--      if debug then write the text into a new file
writeWithBak debug fn textLineType res = 
    if debug 
        then write8 fn textlinesNewFile res
        else do 
            renameToBak8 fn textLineType 
            write8 fn textLineType res     