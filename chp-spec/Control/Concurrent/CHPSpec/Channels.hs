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


-- | The module containing all the different types of channels in CHP.
-- 
-- See <http://chplib.wordpress.com/2010/04/22/automatic-model-generation-part-4-communication/>
-- for details of modelling communication.
module Control.Concurrent.CHPSpec.Channels (
  -- * Channel Creation and Types
  module Control.Concurrent.CHPSpec.Channels.Creation,
  -- * Channel-Ends
  module Control.Concurrent.CHPSpec.Channels.Ends,

  -- * Reading and Writing with Channels
  module Control.Concurrent.CHPSpec.Channels.Communication,

  -- * Useful Type and Function Synonyms
  module Control.Concurrent.CHPSpec.Channels.Synonyms

  )
  where


import Control.Concurrent.CHPSpec.Channels.Base
import Control.Concurrent.CHPSpec.Channels.Communication
import Control.Concurrent.CHPSpec.Channels.Creation
import Control.Concurrent.CHPSpec.Channels.Ends
import Control.Concurrent.CHPSpec.Channels.Synonyms
