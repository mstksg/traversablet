{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.TraversableT where

import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad
import Data.Foldable
import Control.Applicative
import Data.Coerce

newtype TraversableT t m a = TraversableT { runTraversableT :: m (t a) }
                           deriving (Show, Eq, Ord, Enum, Read)

instance (Functor t, Functor m) => Functor (TraversableT t m) where
    fmap f (TraversableT x) = TraversableT $ (fmap . fmap) f x

instance (Applicative t, Applicative m) => Applicative (TraversableT t m) where
    pure = TraversableT . pure . pure
    TraversableT f <*> TraversableT x = TraversableT $ liftA2 (<*>) f x

instance (Monad t, Traversable t, Monad m) => Monad (TraversableT t m) where
    return = TraversableT . pure . pure
    x >>= f = TraversableT $ do
        x' <- runTraversableT x
        let y = runTraversableT . f <$> x'
        join <$> sequence y

instance Applicative t => MonadTrans (TraversableT t) where
    lift = TraversableT . fmap pure

instance (Monad t, Traversable t, MonadIO m) => MonadIO (TraversableT t m) where
    liftIO = TraversableT . fmap pure . liftIO

instance (Traversable t, MonadError e t, Monad m) => MonadError e (TraversableT t m) where
    throwError = TraversableT . return . throwError
    catchError x f = TraversableT $ do
        x' <- runTraversableT x
        let y = sequence $ fmap pure x' `catchError` \e -> pure (f e)
        join <$> runTraversableT y

instance (Alternative t, Traversable t, Monad m) => Alternative (TraversableT t m) where
    empty = TraversableT $ pure empty
    x <|> y = TraversableT $ do
        x' <- runTraversableT x
        let c = sequence $ (return . pure <$> x') <|> pure (runTraversableT y)
        -- c :: m (t (t a))
        -- TODO: is asum okay and not join?
        -- join <$> c
        asum <$> c

instance (MonadPlus t, Traversable t, Monad m) => MonadPlus (TraversableT t m) where
    mzero = empty
    mplus = (<|>)

instance (Foldable t, Foldable m) => Foldable (TraversableT t m) where
    foldMap f (TraversableT x) = foldMap (foldMap f) x

instance (Traversable t, Traversable m) => Traversable (TraversableT t m) where
    traverse f (TraversableT x) = TraversableT <$> traverse (traverse f) x

traversable :: Applicative m => t a -> TraversableT t m a
traversable = TraversableT . pure
