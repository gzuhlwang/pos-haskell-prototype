{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Default implementation of `WithWalletContext`

module Pos.Wallet.Context.Holder
       ( ContextHolder (..)
       , runContextHolder
       ) where

import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader        (ReaderT (ReaderT), ask)

import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadTransfer (..))
import           Control.TimeWarp.Timed      (ThreadId)

import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Slotting                (MonadSlots (..))
import           Pos.Types                   (Timestamp (..))
import           Pos.Wallet.Context.Class    (WithWalletContext (..))
import           Pos.Wallet.Context.Context  (WalletContext (..))

-- | Wrapper for monadic action which brings 'WalletContext'.
newtype ContextHolder m a = ContextHolder
    { getContextHolder :: ReaderT WalletContext m a
<<<<<<< a787abac640ae3b8d825001b069fecf6cc71c49b
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed,
                MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail,
                HasLoggerName, CanLog, MonadDialog s p)
=======
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow,
                MonadCatch, MonadMask, MonadIO, HasLoggerName, CanLog, MonadDialog s p)
>>>>>>> [CSL-447] switch to new tw-sketch, WIP!

-- | Run 'ContextHolder' action.
runContextHolder :: WalletContext -> ContextHolder m a -> m a
runContextHolder ctx = flip runReaderT ctx . getContextHolder

instance Monad m => WrappedM (ContextHolder m) where
    type UnwrappedM (ContextHolder m) = ReaderT WalletContext m
    _WrappedM = iso getContextHolder ContextHolder

instance MonadBase IO m => MonadBase IO (ContextHolder m) where
    liftBase = lift . liftBase

instance MonadTransControl ContextHolder where
    type StT ContextHolder a = StT (ReaderT WalletContext) a
    liftWith = defaultLiftWith ContextHolder getContextHolder
    restoreT = defaultRestoreT ContextHolder

instance MonadBaseControl IO m => MonadBaseControl IO (ContextHolder m) where
    type StM (ContextHolder m) a = ComposeSt ContextHolder m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type instance ThreadId (ContextHolder m) = ThreadId m

instance MonadTransfer s m => MonadTransfer s (ContextHolder m)

instance Monad m => WithWalletContext (ContextHolder m) where
    getWalletContext = ContextHolder ask

instance (Monad m) =>
         MonadSlots (ContextHolder m) where
    getSystemStartTime = ContextHolder $ asks wcSystemStart
    getCurrentTime = Timestamp <$> currentTime
