module Soliloquy.Source
  ( Source(..)
  , readSource
  , SrcSpan(..)
  , mkSrcSpan
  , prettySrcSpan
  ) where

import            Control.Exception         (assert)
import            Text.Megaparsec           (Pos, unPos)
import            Text.Megaparsec.Pos       (SourcePos(SourcePos))
import  qualified Text.PrettyPrint    as P

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

-- | User-friendly 'SrcSpan' presentation.
prettySrcSpan :: SrcSpan -> P.Doc
prettySrcSpan (MkSrcSpan name (bl, bc) (el, ec)) = 
  mconcat 
    [ P.text name, P.colon, P.int $ unPos bl, P.colon, P.int $ unPos bc
    , P.space, P.text "to", P.space
    , P.int $ unPos el, P.colon, P.int $ unPos ec
    ]