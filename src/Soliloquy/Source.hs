module Soliloquy.Source
  ( Source(..)
  , readSource 
  ) where

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