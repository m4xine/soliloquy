module Soliloquy.Source
  ( Source(..)
  , readSource
  , SrcSpan(..)
  , mkSrcSpan
  ) where

import  Control.Exception   (assert)
import  Text.Megaparsec     (Pos)
import  Text.Megaparsec.Pos (SourcePos(SourcePos))

-- | Representation of a Soliloquy source file.
data Source = MkSource
  { sourceName    :: FilePath
  , sourceContent :: Text 
  } 

-- | Attempts to read a source file from the specified file path.
readSource :: MonadIO m => FilePath -> m Source
readSource filePath = do
  fileContent <- liftIO $ readFile filePath
  pure MkSource
    { sourceName    = filePath
    , sourceContent = fileContent
    } 

-- | A specific portion within a source file.
data SrcSpan = MkSrcSpan
  { srcName   :: FilePath
  , srcBegin  :: !(Pos, Pos)
  , srcEnd    :: !(Pos, Pos)
  } deriving Show

-- | Constructs a 'SrcSpan' from two 'SourcePos'.
-- 
-- This will fail if the source names of the provided 'SourcePos'
-- are not the same or if the beginning 'SourcePos' is greater than
-- the ending 'SourcePos'.
mkSrcSpan 
  :: SourcePos -- ^ Beginning
  -> SourcePos -- ^ Ending 
  -> SrcSpan
mkSrcSpan (SourcePos p bl bc) (SourcePos p' el ec) =
  assert (p == p' && ((bl == el && bc <= ec) || (bl < el))) 
    $ MkSrcSpan p (bl, bc) (el, ec)  