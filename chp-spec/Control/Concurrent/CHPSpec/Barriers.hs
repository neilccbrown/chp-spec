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

-- | A module containing barriers.
--
-- The 'PhasedBarrier' type remains, but currently 'syncBarrier' has been changed
-- so that it only works on 'Barrier', i.e. @PhasedBarrier ()@.  This is because
-- phases haven't been modelled yet.
module Control.Concurrent.CHPSpec.Barriers (Barrier, EnrolledBarrier, newBarrier, newBarrierPri, newBarrierWithLabel,
  PhasedBarrier, newPhasedBarrier, newPhasedBarrier', BarOpts(..), defaultIncPhase, defaultBarOpts,
    barLabel, syncBarrier) where

import Control.Monad.Trans

import Control.Concurrent.CHPSpec.Base
import Control.Concurrent.CHPSpec.CSP

type Barrier = PhasedBarrier ()
type EnrolledBarrier = Enrolled PhasedBarrier ()

-- | Unlike normal CHP, this function only works on barriers with the unit type
-- for a phase.
syncBarrier :: EnrolledBarrier -> CHP ()
syncBarrier (Enrolled (Barrier x)) = fakeCommBarr x
    
data BarOpts phase = BarOpts { barIncPhase :: phase -> phase
  , barPriority :: Int
  , barOptsShow :: phase -> String
  , barOptsLabel :: Maybe String }

defaultIncPhase :: (Enum phase, Bounded phase, Eq phase) => phase -> phase
defaultIncPhase p
  | p == maxBound = minBound
  | otherwise = succ p

defaultBarOpts :: (Enum phase, Bounded phase, Eq phase) => BarOpts phase
defaultBarOpts = BarOpts defaultIncPhase 0 (const "") Nothing

barLabel :: (Enum phase, Bounded phase, Eq phase, Show phase) => String -> BarOpts phase
barLabel = BarOpts defaultIncPhase 0 show . Just

newBarrier :: CHP Barrier
newBarrier = newBarrierPri 0

newBarrierPri :: Int -> CHP Barrier
newBarrierPri n = newPhasedBarrier' () $ BarOpts (const ()) n (const "") Nothing

newPhasedBarrier :: (Enum phase, Bounded phase, Eq phase, Show phase) => phase -> CHP (PhasedBarrier phase)
newPhasedBarrier ph = newPhasedBarrier' ph $ BarOpts defaultIncPhase 0 show Nothing

newPhasedBarrier' :: phase -> BarOpts phase -> CHP (PhasedBarrier phase)
newPhasedBarrier' _ o = do e <- lift newEvent
                           maybe (return ()) (lift . labelEvent e) $ barOptsLabel o
                           return $ Barrier e

newBarrierWithLabel :: String -> CHP Barrier
newBarrierWithLabel = newPhasedBarrier' () . BarOpts (const ()) 0 (const "") . Just

