module Soliloquy.Parser.Phase2.Internal
  ( P2T(..)
  , runP2T
  , parentSrc
  , match
  , parseError
  , P2
  , runP2
  , MatchListT(..)
  , runMatchListT
  , chomp
  , chomps
  , MatchList
  , runMatchList
  ) where

import  Control.Monad.Error.Class (liftEither)
import  Control.Monad.Trans.Class (MonadTrans)
import  Control.Monad.Loops       (untilM)
import  Control.Comonad.Cofree    (Cofree((:<)))
import  Data.Composition          ((.:))
import  Soliloquy.Source          (SrcSpan)
import  Soliloquy.Syntax.Obj      (SrcObj, ListKind, ObjF(..))
import  Soliloquy.Parser.Error    (ParseError (..))

-- | Second parsing pass monad transformer.
newtype P2T m a = MkP2T
  { unP2T :: ExceptT [ParseError] (ReaderT SrcObj m) a
  } deriving
      ( Functor
      , Applicative
      , Alternative
      , Monad
      , MonadError [ParseError]
      , MonadReader SrcObj
      )

instance MonadTrans P2T where
  lift :: Monad m => m a -> P2T m a
  lift = MkP2T . lift . lift

-- | Runs a @P2T@ monad transformer with the given parent object.
runP2T :: Monad m => P2T m a -> SrcObj -> m (Either [ParseError] a)
runP2T (MkP2T p) = runReaderT $ runExceptT p

parentSrc :: Monad m => P2T m SrcSpan
parentSrc = do
  src :< _ <- ask
  pure src 

-- | Match against the parent object. 
match :: Monad m => (SrcObj -> P2T m a) -> P2T m a
match = (ask >>=)

-- | Throw a parser at the parent object's 
-- source location with a custom error message.
parseError :: Monad m => Text -> P2T m a
parseError msg = do
  src <- parentSrc
  throwError [P2Error src msg]   

type P2 = P2T Identity

runP2 :: P2 a -> SrcObj -> Either [ParseError] a
runP2 = runIdentity .: runP2T 

-- | DSL to match against Soliloquy list objects. 
newtype MatchListT m a = MkMatchListT
  { unChunksT :: StateT [SrcObj] (P2T m) a
  } deriving
      ( Functor
      , Applicative
      , Monad
      , MonadState [SrcObj]
      , MonadError [ParseError]
      , MonadReader SrcObj
      )

instance MonadTrans MatchListT where
  lift :: Monad m => m a -> MatchListT m a
  lift = MkMatchListT . lift . lift 

-- | Match against a list object with a @MatchListT@. 
runMatchListT :: Monad m => ListKind -> MatchListT m a -> P2T m a 
runMatchListT kind (MkMatchListT m) = ask >>= \case 
  -- TODO: Error if list is not entirely consumed (runStateT)
  _ :< OList k xs | k == kind -> evalStateT m xs 
  _ -> parseError "Expected list object"

-- | Removes the head of the parent list and matches 
-- against the object with the provided parser. 
chomp :: Monad m => P2T m a -> MatchListT m a
chomp p = get >>= \case
  x:xs -> do
    put xs
    lift (runP2T p x) >>= liftEither
  [] -> MkMatchListT . lift $ parseError "Expected additional child"

-- | Chomps with the provided parser until the parser fails
-- or the parent list is empty. 
chomps :: Monad m => P2T m a -> MatchListT m [a]
chomps p = untilM (chomp p) (gets null)

type MatchList = MatchListT Identity

runMatchList :: ListKind -> MatchList a -> P2 a
runMatchList = runMatchListT