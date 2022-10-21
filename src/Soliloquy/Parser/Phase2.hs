module Soliloquy.Parser.Phase2 
  ( p2
  ) where

import  Prelude                           hiding  (sym, Type)
import  Control.Monad.Combinators                 (choice)
import  Control.Comonad.Cofree                    (Cofree((:<)))
import  Soliloquy.Parser.Phase2.Internal          (P2, runMatchList, match, chomp, parentSrc, chomps, parseError, runP2, runMatchListT, chomps1)
import  Soliloquy.Parser.Error                    (ParseError)
import  Soliloquy.Syntax                          (PsToplevel, PsExpr, Toplevel (..), Expr(..), StringLit (..), PsPat, Pat (..))
import  Soliloquy.Syntax.Path                     (Path (..))
import  Soliloquy.Syntax.Obj                      (SrcObj, ListKind (..), ObjF (..))
import  Soliloquy.Type                            (Type (..))

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
  _ :< OSym s -> pure $ MkPath (s :| [])
  _ -> parseError "Expected path" 

string :: P2 Text
string = match $ \case
  _ :< OString s -> pure s
  _ -> parseError "Expected string"

typeString :: P2 Type
typeString = matchSym "String" $> TString 

typeList :: P2 Type
typeList = runMatchListT ParenList $ do
  chomp $ matchSym "List"
  TList <$> chomp type'

typeVar :: P2 Type
typeVar = TVar <$> path

type' :: P2 Type
type' = choice [typeString, typeList, typeVar]

patVar :: P2 PsPat
patVar = match $ \case
  ann :< OSym s -> pure $ PVar s ann
  _ -> parseError "Expected pattern variable"

patSig :: P2 PsPat
patSig = match $ \case
  ann :< OSym s -> pure $ PSig (PVar s ann) TDynamic ann 
  ann :< OList ParenList _ -> 
    runMatchList ParenList $ do
      pat' <- chomp pat
      type'' <- chomp type'
      pure $ PSig pat' type'' ann 
  _ -> parseError "Expected pattern signature"

pat :: P2 PsPat 
pat = choice [patVar, patSig] 

stringLit :: P2 StringLit
stringLit = StringLitText <$> string

exprVar :: P2 PsExpr
exprVar = EVar <$> parentSrc <*> path

exprString :: P2 PsExpr
exprString = EString <$> parentSrc <*> stringLit

exprList :: P2 PsExpr
exprList = 
  EList <$> parentSrc <*> runMatchList BracketList (chomps expr)

exprInvoke :: P2 PsExpr
exprInvoke = do
  src <- parentSrc
  runMatchList ParenList $ do
    EInvoke src <$> chomp expr <*> chomps1 expr 

expr :: P2 PsExpr
expr = choice [exprVar, exprString, exprList, exprInvoke] 

tlDefVal :: P2 PsToplevel
tlDefVal = do
  src <- parentSrc
  runMatchList ParenList $ do
    chomp $ matchSym "def"
    name <- chomp sym
    body <- chomp expr
    pure $ TLDefVal src name body

tlDefFun :: P2 PsToplevel
tlDefFun = do
  src <- parentSrc
  runMatchList ParenList $ do
    chomp $ matchSym "defn"
    name <- chomp sym
    params <- chomp . runMatchList ParenList $ chomps pat
    body <- chomps1 expr
    pure $ TLDefFun src name params body 

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
toplevel = choice [tlDefVal, tlDefFun, tlDeclMod, tlImport] 

-- | Parses a list of Soliloquy objects into a concise syntax representation.
p2 :: [SrcObj] -> Either [ParseError] [PsToplevel]
p2 objs = forM objs $ runP2 toplevel