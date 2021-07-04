-- Communicating Haskell Processes.
-- Copyright (c) 2008--2009, University of Kent.
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--  * Neither the name of the University of Kent nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module Control.Concurrent.CHPSpec.Channels.Creation (
  Chan, Channel(..), newChannel, ChanOpts(..), defaultChanOpts, chanLabel, newChannelWR, newChannelRW,
  newChannelList, newChannelListWithLabels, newChannelListWithStem,
  labelChannel
  ) where

import Control.Monad
import Control.Monad.Trans

import Control.Concurrent.CHPSpec.Base
import Control.Concurrent.CHPSpec.Channels.Base
import Control.Concurrent.CHPSpec.CSP
import Control.Concurrent.CHPSpec.Spec

class Channel r w where
  newChannel' :: MonadCHP m => ChanOpts a -> m (Chan r w a)
  sameChannel :: r a -> w a -> Bool

-- | Options for channel creation.  The first two will be ignored, but the label
-- (if present) will be used to label the channel in the specification.
data ChanOpts a = ChanOpts {
  chanOptsPriority :: Int,
  chanOptsShow :: a -> String,
  chanOptsLabel :: Maybe String }

defaultChanOpts :: ChanOpts a
defaultChanOpts = ChanOpts 0 (const "") Nothing

chanLabel :: Show a => String -> ChanOpts a
chanLabel = ChanOpts 0 show . Just

newChannel :: (MonadCHP m, Channel r w) => m (Chan r w a)
newChannel = newChannel' defaultChanOpts

newChannelRW :: (Channel r w, MonadCHP m) => m (r a, w a)
newChannelRW = do c <- newChannel
                  return (reader c, writer c)

newChannelWR :: (Channel r w, MonadCHP m) => m (w a, r a)
newChannelWR = do c <- newChannel
                  return (writer c, reader c)

newChannelList :: (Channel r w, MonadCHP m) => Int -> m [Chan r w a]
newChannelList n = replicateM n newChannel

newChannelListWithStem :: (Channel r w, MonadCHP m) => Int -> String -> m [Chan r w a]
newChannelListWithStem n s = sequence [newChannel' $ ChanOpts 0 (const "") (Just $ s ++ show i) | i <- [0 .. (n - 1)]]

newChannelListWithLabels :: (Channel r w, MonadCHP m) => [String] -> m [Chan r w a]
newChannelListWithLabels = mapM (newChannel' . ChanOpts 0 (const "") . Just)


-- | Labels a channel in the traces.  It is easiest to do this at creation.
-- The effect of re-labelling channels after their first use is undefined.
--
-- This function does work as expected in chp-spec.
labelChannel :: MonadCHP m => Chan r w a -> String -> m ()
labelChannel c = liftCHP . lift . labelEvent (getChannelIdentifier c)


instance Channel Chanin Chanout where
  newChannel' o = do c <- chan (stmChannel) Chanin Chanout
                     maybe (return ()) (labelChannel c) (chanOptsLabel o)
                     return c
  sameChannel (Chanin x) (Chanout y) = x == y

instance Channel (Shared Chanin) Chanout where
  newChannel' o = do
                  c <- newChannel' o
                  return $ Chan (getChannelIdentifier c) (Shared (reader c)) (writer c)
  sameChannel (Shared (Chanin x)) (Chanout y) = x == y

instance Channel Chanin (Shared Chanout) where
  newChannel' o = do
                  c <- newChannel' o
                  return $ Chan (getChannelIdentifier c) (reader c) (Shared (writer c))
  sameChannel (Chanin x) (Shared (Chanout y)) = x == y

instance Channel (Shared Chanin) (Shared Chanout) where
  newChannel' o = do
                  c <- newChannel' o
                  return $ Chan (getChannelIdentifier c) (Shared (reader c)) (Shared (writer c))
  sameChannel (Shared (Chanin x)) (Shared (Chanout y)) = x == y

chan :: Monad m => m (EventId, c a) -> (c a -> r a) -> (c a -> w a) -> m (Chan r w a)
chan m r w = do (u, x) <- m
                return $ Chan u (r x) (w x)
