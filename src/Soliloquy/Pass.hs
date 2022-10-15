module Soliloquy.Pass
  ( Pass(..)
  , Ps
  ) where

data SqPass = SqParsed 

-- | A compiler pass. Used to annotate type families
--  within distinct compiler phases.
data Pass (p :: SqPass) where
  Parsed :: Pass 'SqParsed

type Ps = Pass 'SqParsed 