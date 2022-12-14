module Soliloquy.Parser.Phase1 
  ( p1
  ) where

import            Prelude                     hiding  (sym, try, ignore)
import            Control.Comonad.Cofree              (Cofree((:<)))
import            Data.String                         (fromString)
import            Data.Composition                    ((.:))
import            Text.Megaparsec                     (Parsec, getSourcePos, oneOf, anySingle, manyTill, choice, between, eof, parse, sepBy1, MonadParsec (try), sepBy)
import            Text.Megaparsec.Char                (space, numberChar, letterChar, char, string, space1)
import  qualified Text.Megaparsec.Char.Lexer  as L
import            Soliloquy.Source                    (Source(MkSource), mkSrcSpan, SrcSpan)
import            Soliloquy.Syntax.Obj                (SrcObj, ObjF(..), ListKind(..))
import            Soliloquy.Parser.Error              (ParseError(P1Error))
import            Soliloquy.Syntax.Path               (Path(..))

type P1 = Parsec Void Text 

ignore :: P1 () 
ignore = L.space 
  space1 
  (L.skipLineComment "#") 
  (L.skipBlockComment "#|" "|#")

l :: P1 a -> P1 a
l = L.lexeme ignore

s :: Text -> P1  Text
s = string

ls :: Text -> P1 Text
ls = l . s

cosrc :: P1 (f (Cofree f SrcSpan)) -> P1 (Cofree f SrcSpan)
cosrc p = do
  begin <- getSourcePos
  f     <- p
  end   <- getSourcePos
  pure $ mkSrcSpan begin end :< f 

sym :: P1 Text
sym = 
    (fromString .: (:)) <$> head <*> many tail
  where 
    head = letterChar <|> oneOf ("_-~*!" :: [Char])
    tail = head <|> numberChar

path :: P1 Path
path = (MkPath .: (:|)) 
  <$> (sym <* char '.') 
  <*> sepBy sym (char '.')

objSym :: P1 SrcObj
objSym = cosrc $ OSym <$> sym 

objPath :: P1 SrcObj
objPath = cosrc $ OPath <$> path 

objString :: P1 SrcObj
objString =
  cosrc $ OString . fromString <$> do 
    char '"' *> manyTill anySingle (char '"')

objList :: P1 SrcObj
objList = 
    cosrc . choice $ f <$> parens
  where
    obj = l $ choice [try objPath, objSym, objString, objList]
    parens = 
      [ ("{", "}", CurlyList)
      , ("[", "]", BracketList)
      , ("(", ")", ParenList)
      ]
    f (l, r, k) = between (ls l) (s r) $ OList k <$> many obj

-- | Parses the provided Soliloquy source file into objects.
p1 :: Source -> Either ParseError [SrcObj]
p1 (MkSource sourceName sourceContent) = 
  first P1Error $ parse (ignore *> manyTill (l objList) eof) sourceName sourceContent 