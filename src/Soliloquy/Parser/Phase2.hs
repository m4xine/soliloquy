module Soliloquy.Parser.Phase2 
  ( p2
  ) where

import  Prelude                           hiding  (sym)
import  Control.Monad.Combinators                 (choice)
import  Control.Comonad.Cofree                    (Cofree((:<)))
import  Soliloquy.Parser.Phase2.Internal          (P2, runMatchList, match, chomp, parentSrc, chomps, parseError, runP2)
import  Soliloquy.Parser.Error                    (ParseError)
import  Soliloquy.Syntax                          (PsToplevel, PsExpr, Toplevel (..), Expr(..), StringLit (..))
import  Soliloquy.Syntax.Sym                      (Sym, mkSym1)
import  Soliloquy.Syntax.Obj                      (SrcObj, ListKind (..), ObjF (..))

sym :: P2 Sym
sym = match $ \case
  _ :< OSym s -> pure s
  _ -> parseError "Expected symbol" 

matchSym :: Sym -> P2 Sym
matchSym kw = 
  ifM ((kw ==) <$> sym) 
    (pure kw) 
    (parseError $ "Expected keyword '" <> show kw <> "'")

matchSym1 :: Text -> P2 Sym
matchSym1 = matchSym . mkSym1

string :: P2 Text
string = match $ \case
  _ :< OString s -> pure s
  _ -> parseError "Expected string"

stringLit :: P2 StringLit
stringLit = StringLitText <$> string

exprVar :: P2 PsExpr
exprVar = EVar <$> parentSrc <*> sym

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
    chomp $ matchSym1 "def"
    name <- chomp sym
    body <- chomp expr
    pure $ TLDefVal src name body

tlDeclMod :: P2 PsToplevel
tlDeclMod = do
  src <- parentSrc
  runMatchList ParenList $ do
    chomp $ matchSym1 "module"
    name <- chomp sym
    pure $ TLDeclMod src name

tlImport :: P2 PsToplevel
tlImport = do
  src <- parentSrc
  let import' = runMatchList ParenList $ do
        chomp $ matchSym1 "import"
        sym' <- chomp sym
        pure $ TLImport src sym' Nothing
  let qualifiedImport = runMatchList ParenList $ do
        chomp $ matchSym1 "import-as"
        sym' <- chomp sym
        qualifier <- chomp sym 
        pure . TLImport src sym' $ Just qualifier
  import' <|> qualifiedImport

toplevel :: P2 PsToplevel
toplevel = choice [tlDefVal, tlDeclMod, tlImport] 

-- | Parses a list of Soliloquy objects into a concise syntax representation.
p2 :: [SrcObj] -> Either [ParseError] [PsToplevel]
p2 objs = forM objs $ runP2 toplevel