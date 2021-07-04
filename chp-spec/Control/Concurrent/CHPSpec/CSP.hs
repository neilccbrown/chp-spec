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

-- | A module containing a few miscellaneous items that can't go in Control.Concurrent.CHP.Base
-- because they would form a cyclic module link.  Not publicly visible.
-- TODO rename this module.
module Control.Concurrent.CHPSpec.CSP where

import qualified Control.Exception as C
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Typeable

import Control.Concurrent.CHPSpec.Base
import Control.Concurrent.CHPSpec.Enroll
import Control.Concurrent.CHPSpec.Spec

{-
-- | Synchronises on the given barrier.  You must be enrolled on a barrier in order
-- to synchronise on it.  Returns the new phase, following the synchronisation.
syncBarrierWith :: (Unique -> (Unique -> Integer) -> String -> [RecordedIndivEvent Unique])
  -> (Int -> STM ()) -> Enrolled PhasedBarrier phase -> CHP phase
syncBarrierWith recE storeN (Enrolled (Barrier (e,tv, fph)))
    = buildOnEventPoison (wrapIndiv recE) e (EventActions incPhase (return ()))
        (NoPoison <$> (atomically $ readTVar tv))
    where
      incPhase :: Map.Map Unique Int -> STM ()
      incPhase m = do readTVar tv >>= writeTVar tv . fph
                      maybe (return ()) storeN $ Map.lookup (Event.getEventUnique e) m

-}

data PhasedBarrier phase = Barrier EventId
  deriving (Eq, Typeable)

instance Enrollable PhasedBarrier phase where
  enroll b f
    = f $ Enrolled b
  resign _ m = m

bottomPrefix :: String
bottomPrefix = "__CHP.bottom__"

fakeCommIn :: Integer -> CHP a
fakeCommIn n = addSpecT1 $ do
  st <- get
  put $ st { chpNextBottom = succ $ chpNextBottom st }
  return (error $ bottomPrefix ++ show (chpNextBottom st)
         ,Sync $ Right (n, DirInput, chpNextBottom st)
         )

fakeCommOut :: Integer -> a -> CHP ()
fakeCommOut n x = addSpecT1 $ do
  possErr <- lift $ C.try $ C.evaluate x
  case possErr of
    Left (C.ErrorCall s) | bottomPrefix `L.isPrefixOf` s
      -> return ((), Sync $ Right
           (n, DirOutput, read $ L.drop (L.length bottomPrefix) s))
    -- Wasn't one of our bottoms, we can't know anything about it, hope this will work in FDR:
    _ -> return ((), Sync $ Left n)

fakeCommBarr :: Integer -> CHP a
fakeCommBarr n = addSpecT1 $ return (undefined, Sync $ Left n)

-- TODO this method will give a different channel each time, but at the moment
-- we treat it as if it will always act the same, so for example with recursive
-- processes (such as the sieve), it won't reflect the behaviour correctly
newEvent :: StateT CHPState IO EventId
newEvent = do       st <- get
                    let n = toInteger $ Map.size $ chpEventMap st
                    put $ st { chpEventMap = Map.insert n "" $ chpEventMap st }
                    return n

labelEvent :: EventId -> String -> StateT CHPState IO ()
labelEvent n s = modify $ \st -> st { chpEventMap = Map.insert n s $ chpEventMap st }

newtype Shared c a = Shared (c a)
