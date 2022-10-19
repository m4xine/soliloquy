module Soliloquy.Parser.Phase2 
  ( p2
  ) where

import  Prelude                           hiding  (sym)
import  Control.Monad.Combinators                 (choice)
import  Control.Comonad.Cofree                    (Cofree((:<)))
import  Soliloquy.Parser.Phase2.Internal          (P2, runMatchList, match, chomp, parentSrc, chomps, parseError, runP2)
import  Soliloquy.Parser.Error                    (ParseError)
import  Soliloquy.Syntax                          (PsToplevel, PsExpr, Toplevel (..), Expr(..), StringLit (..))
import  Soliloquy.Syntax.Path                     (Path)
import  Soliloquy.Syntax.Obj                      (SrcObj, ListKind (..), ObjF (..))

sym :: P2 Text
sym = match $ \case
  _ :< OSym s -> pure s
  _ -> parseError "Expected symbol"

matchSym :: Text -> P2 Text
matchSym s = match $ \case 
  _ :< OSym s' | s == s' -> pure s'
  _ -> parseError $ "Expected symbol '" <> s <> "'"

path :: P2 Path
path = match $ \case
  _ :< OPath p -> pure p
  _ -> parseError "Expected path" 

string :: P2 Text
string = match $ \case
  _ :< OString s -> pure s
  _ -> parseError "Expected string"

stringLit :: P2 StringLit
stringLit = StringLitText <$> string

exprVar :: P2 PsExpr
exprVar = EVar <$> parentSrc <*> path

exprString :: P2 PsExpr
exprString = EString <$> parentSrc <*> stringLit

exprList :: P2 PsExpr
exprList = 
  EList <$> parentSrc <*> runMatchList BracketList (chomps expr)

expr :: P2 PsExpr
expr = choice [ exprVar, exprString, exprList ] 

tlDefVal :: P2 PsToplevel
tlDefVal = do
  src <- parentSrc
  runMatchList ParenList $ do
    chomp $ matchSym "def"
    name <- chomp sym
    body <- chomp expr
    pure $ TLDefVal src name body

tlDeclMod :: P2 PsToplevel
tlDeclMod = do
  src <- parentSrc
  runMatchList ParenList $ do
    chomp $ matchSym "module"
    name <- chomp sym
    pure $ TLDeclMod src name

tlImport :: P2 PsToplevel
tlImport = do
  src <- parentSrc
  let import' = runMatchList ParenList $ do
        chomp $ matchSym "import"
        modPath <- chomp path
        pure $ TLImport src modPath Nothing
  let qualifiedImport = runMatchList ParenList $ do
        chomp $ matchSym "import-as"
        modPath <- chomp path
        modQual <- chomp path 
        pure . TLImport src modPath $ Just modQual
  import' <|> qualifiedImport

toplevel :: P2 PsToplevel
toplevel = choice [tlDefVal, tlDeclMod, tlImport] 

-- | Parses a list of Soliloquy objects into a concise syntax representation.
p2 :: [SrcObj] -> Either [ParseError] [PsToplevel]
p2 objs = forM objs $ runP2 toplevel