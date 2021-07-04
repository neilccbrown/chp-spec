-- Communicating Haskell Processes.
-- Copyright (c) 2008-2010, University of Kent, Neil Brown.
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


-- | A module containing the choice constructs.  See <http://chplib.wordpress.com/2010/04/20/automatic-model-generation-part-3-choice-and-io/>
-- for details of how choice is modelled.
--
-- Currently conjunction is not modelled (mainly because CSP\/FDR don't support
-- it), but external choice is modelled fine; 'priAlt' is modelled as a plain 'alt',
-- though.
module Control.Concurrent.CHPSpec.Alt (alt, (<->), priAlt, (</>)) where

import Control.Concurrent.CHPSpec.Base
import Control.Concurrent.CHPSpec.Spec

alt :: [CHP a] -> CHP a
alt = priAlt

priAlt :: [CHP a] -> CHP a
priAlt [] = stopSpecT $ return Stop
priAlt ps = altSpecT ps

(<->) :: CHP a -> CHP a -> CHP a
(<->) a b = alt [a,b]
  
(</>) :: CHP a -> CHP a -> CHP a
(</>) a b = priAlt [a,b]

infixl </>
infixl <->

