module Soliloquy.Parser
  ( parse 
  ) where

import  Soliloquy.Source        (Source)
import  Soliloquy.Syntax        (PsToplevel)
import  Soliloquy.Parser.Error  (ParseError)
import  Soliloquy.Parser.Phase1 (p1)
import  Soliloquy.Parser.Phase2 (p2)

-- | Parses a Soliloquy source file.
parse :: Source -> Either [ParseError] [PsToplevel]
parse src = first (:[]) (p1 src) >>= p2 