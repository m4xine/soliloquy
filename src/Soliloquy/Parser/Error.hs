module Soliloquy.Parser.Error
  ( ParseError(..)
  , printParseError
  ) where

import  Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

data ParseError 
  = P1Error (ParseErrorBundle Text Void)
  deriving Show 

printParseError :: MonadIO m => ParseError -> m ()
printParseError (P1Error bundle) = {- TEMP -} putStrLn $ errorBundlePretty bundle