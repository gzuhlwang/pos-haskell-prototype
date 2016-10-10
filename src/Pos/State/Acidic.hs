{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Acid-state wrapped operations.

module Pos.State.Acidic
       ( NodeState
       , closeState
       , openState
       , openMemState
       , tidyState

       , query
       , update

       , CreateBlock (..)
       , AddLeaders (..)
       , GetLeader (..)
       , GetLeaders (..)
       , AddEntry (..)
       , AdoptBlock (..)
       , SetLeaders (..)
       ) where

import           Data.Acid          ()
import           Universum

import           Data.Acid          (EventResult, EventState, Query, QueryEvent, Update,
                                     UpdateEvent, makeAcidic)
import           Data.Default       (def)

import           Serokell.AcidState (ExtendedState, closeExtendedState,
                                     openLocalExtendedState, openMemoryExtendedState,
                                     queryExtended, tidyExtendedState, updateExtended)

import           Pos.State.Storage  (Storage)
import qualified Pos.State.Storage  as S
import           Pos.Types

----------------------------------------------------------------------------
-- Acid-state things
----------------------------------------------------------------------------

type NodeState = ExtendedState Storage

query
    :: (EventState event ~ Storage, QueryEvent event, MonadIO m)
    => NodeState -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ Storage, UpdateEvent event, MonadIO m)
    => NodeState -> event -> m (EventResult event)
update = updateExtended

openState :: MonadIO m => Bool -> FilePath -> m NodeState
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp def

openMemState :: MonadIO m => m NodeState
openMemState = openMemoryExtendedState def

closeState :: MonadIO m => NodeState -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => NodeState -> m ()
tidyState = tidyExtendedState

----------------------------------------------------------------------------
-- Redeclarations of operations
----------------------------------------------------------------------------

-- 'makeAcidic' demands that operations use Update and Query from acid-state,
-- even if our Update and Query are supersets of those (i.e. our Update is
-- “MonadState m => m a” and acid-state's Update is an instance of MonadState
-- so it fits). Hence we have to redefine all operations in order to be able
-- to use 'makeAcidic' on them.

createBlock :: Update Storage Blockkk
createBlock = S.createBlock

addLeaders :: Int -> [NodeId] -> Update Storage ()
addLeaders = S.addLeaders

getLeader :: Int -> Int -> Query Storage (Maybe NodeId)
getLeader = S.getLeader

getLeaders :: Int -> Query Storage (Maybe [NodeId])
getLeaders = S.getLeaders

addEntry :: Entry -> Update Storage ()
addEntry = S.addEntry

adoptBlock :: Blockkk -> Update Storage ()
adoptBlock = S.adoptBlock

setLeaders :: Int -> [NodeId] -> Update Storage ()
setLeaders = S.setLeaders

makeAcidic ''Storage
    [ 'createBlock
    , 'addLeaders
    , 'getLeader
    , 'getLeaders
    , 'addEntry
    , 'adoptBlock
    , 'setLeaders
    ]