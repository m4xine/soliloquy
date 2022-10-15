module Soliloquy.Parser.Error
  ( ParseError(..)
  , printParseError
  ) where

import  Text.Megaparsec   (ParseErrorBundle, errorBundlePretty)
import  Soliloquy.Source  (SrcSpan, prettySrcSpan)
import Data.String (fromString)
import qualified Text.PrettyPrint as P
import qualified Data.Text as T

-- | An error encountered within the first or second phase of parsing.
data ParseError 
  = P1Error (ParseErrorBundle Text Void)
  | P2Error 
      { p2errorSrcSpan :: SrcSpan
      , p2errorMessage :: Text
      }
  deriving Show

-- | Prints a user-friendly 'ParseError'.
printParseError :: MonadIO m => ParseError -> m ()
printParseError (P1Error bundle) = {- TEMP -} putStrLn $ errorBundlePretty bundle
printParseError (P2Error span msg) = 
    {- TEMP -} putStrLn $ P.render doc
  where
    doc = mconcat [ prettySrcSpan span, P.colon, P.space, P.text $ T.unpack msg ]