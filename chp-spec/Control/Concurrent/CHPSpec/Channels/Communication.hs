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

module Control.Concurrent.CHPSpec.Channels.Communication (
  ReadableChannel(..), WriteableChannel(..), writeValue, writeChannelStrict
  ) where

import Control.DeepSeq
import Control.Monad

import Control.Concurrent.CHPSpec.Base
import Control.Concurrent.CHPSpec.CSP
import Control.Concurrent.CHPSpec.Channels.Base

class ReadableChannel chanEnd where -- minimal implementation: extReadChannel
  readChannel :: chanEnd a -> CHP a
  readChannel c = extReadChannel c return
  -- | Currently, extended inputs and outputs are modelled (incorrectly) as standard
  -- inputs and outputs.
  extReadChannel :: chanEnd a -> (a -> CHP b) -> CHP b

class WriteableChannel chanEnd where -- minimal implementation: extWriteChannel
  writeChannel :: chanEnd a -> a -> CHP ()
  writeChannel c x = extWriteChannel c (return x) >> return ()

  -- | Currently, extended inputs and outputs are modelled (incorrectly) as standard
  -- inputs and outputs.
  extWriteChannel :: chanEnd a -> CHP a -> CHP ()
  extWriteChannel c = extWriteChannel' c . liftM (flip (,) ())

  -- | Currently, extended inputs and outputs are modelled (incorrectly) as standard
  -- inputs and outputs.
  extWriteChannel' :: chanEnd a -> CHP (a, b) -> CHP b
  

-- ==========
-- Functions: 
-- ==========

writeValue :: WriteableChannel chanEnd => a -> chanEnd a -> CHP ()
writeValue = flip writeChannel

-- | This function strictly evaluates its second argument and then behaves like
-- 'writeChannel'.
writeChannelStrict :: (NFData a, WriteableChannel chanEnd) => chanEnd a -> a -> CHP ()
writeChannelStrict c x = case rnf x of () -> writeChannel c x

-- ==========
-- Instances: 
-- ==========

instance ReadableChannel Chanin where
  readChannel (Chanin (STMChan c)) = fakeCommIn c

  extReadChannel (Chanin (STMChan c)) body
    = fakeCommIn c >>= body

instance WriteableChannel Chanout where
  writeChannel (Chanout (STMChan c)) x
    = fakeCommOut c x >> return ()
  extWriteChannel (Chanout (STMChan c)) body
    = body >>= fakeCommOut c >> return ()
  extWriteChannel' (Chanout (STMChan c)) body
    = body >>= \(val, ret) -> fakeCommOut c val >> return ret
