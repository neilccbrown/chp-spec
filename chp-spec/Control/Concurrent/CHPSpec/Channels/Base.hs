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

module Control.Concurrent.CHPSpec.Channels.Base where

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans
import Data.Typeable

import Control.Concurrent.CHPSpec.Base
import Control.Concurrent.CHPSpec.CSP
import Control.Concurrent.CHPSpec.Spec

newtype Chanin a = Chanin (STMChannel a) deriving (Eq, Typeable)

newtype Chanout a = Chanout (STMChannel a) deriving (Eq, Typeable)

newtype STMChannel a = STMChan EventId
  deriving (Eq, Typeable)

data Chan r w a = Chan {
  -- | Gets the channel's identifier.  Useful if you need to be able to identify
  -- a channel in the trace later on.
  getChannelIdentifier :: EventId,
  reader :: r a,
  writer :: w a}

instance Poisonable (Chanin a) where
  poison (Chanin _) = return ()
  checkForPoison (Chanin _) = return ()

instance Poisonable (Chanout a) where
  poison (Chanout _) = return ()
  checkForPoison (Chanout _) = return ()

stmChannel :: MonadCHP m => m (EventId, STMChannel a)
stmChannel = liftM (id &&& STMChan) $ liftCHP $ lift newEvent

