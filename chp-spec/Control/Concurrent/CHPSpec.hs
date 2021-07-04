-- Communicating Haskell Processes.
-- Copyright (c) 2008, University of Kent.
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

-- | This module re-exports all of the functionality of the chp-spec library, except
-- the "Control.Concurrent.CHPSpec.LazySmallCheck" module.
--
-- The documentation for this library may seem relatively spartan; only the functions
-- and modules with significant differences\/caveats from CHP API are documented.
--  All the details of how each aspect is modelled is available in the original
-- series of blog posts.
module Control.Concurrent.CHPSpec (
  module Control.Concurrent.CHPSpec.Alt,
  module Control.Concurrent.CHPSpec.Barriers,
  module Control.Concurrent.CHPSpec.Channels,
  module Control.Concurrent.CHPSpec.Enroll,
  module Control.Concurrent.CHPSpec.Monad,
  module Control.Concurrent.CHPSpec.Parallel
  ) where

import Control.Concurrent.CHPSpec.Alt
import Control.Concurrent.CHPSpec.Barriers
import Control.Concurrent.CHPSpec.Channels
import Control.Concurrent.CHPSpec.Enroll
import Control.Concurrent.CHPSpec.Monad
import Control.Concurrent.CHPSpec.Parallel




