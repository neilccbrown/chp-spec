-- Communicating Haskell Processes.
-- Copyright (c) 2009-2010, Neil Brown.
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
--  * Neither the name of the Neil Brown nor the names of other
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

-- | A module providing the process annotation for recursive processes.
module Control.Concurrent.CHPSpec.Process (Process, process, subProcess) where

import Control.Applicative
import Control.Monad.State
import Data.Dynamic (Dynamic, fromDyn, fromDynamic, toDyn)
import qualified Data.List as L
import Data.Typeable (Typeable)
import qualified Data.Map as Map

import Control.Concurrent.CHPSpec.Base
import Control.Concurrent.CHPSpec.Spec

checkArgs :: [Dynamic] -> [Dynamic -> Bool] -> Bool
checkArgs ds fs
  | length ds /= length fs = False
  | otherwise = and $ zipWith ($) fs ds

-- | A class with instances for CHP processes of the form @a -> b -> .. -> CHP r@.
--
-- The return value of the process must support 'Typeable', and the arguments of
-- the process must support 'Typeable' and 'Eq'.
class Process p where
  -- Internal use only:
  process' :: Bool -> String -> [(Dynamic, CheckArg)] -> p -> p

instance (Typeable a) => Process (CHP a) where
  process' topLevel name immArgs p = addSpecT1 $ do
    st <- get
    let possibles = Map.toList <$> (Map.lookup name $ chpProcessMap st)
        args = if topLevel then immArgs
                 else chpFreeNames st ++ immArgs
    case possibles >>= L.find (checkArgs (map fst args) . fst . snd) of
      Just (n, (_, (_, r)))
        -> return (flip fromDyn (error "process-lookup") r, Call n)
      Nothing -> 
        do let n = chpNextProcess st
           put $ st { chpProcessMap = insertMapMap name n
                        (map snd args, (error "process", toDyn ())) $ chpProcessMap st
                    , chpFreeNames = args
                    , chpNextProcess = succ n
                    }
           (r, f) <- finSpecT p
           modify $ \st' -> st' { chpProcessMap = insertMapMap name n
                                    (map snd args, (f, toDyn r)) $ chpProcessMap st'
                                  -- Restore original free names:
                                , chpFreeNames = chpFreeNames st }
           return (r, Call n)

instance (Eq a, Typeable a, Process b) => Process (a -> b) where
  process' topLevel name args f x
    = process' topLevel name (args ++ [(toDyn x, (== Just x) . fromDynamic)]) (f x)

-- | An annotation to put around a top-level process.  This annotation must be
-- inside the recursive knot.  You can either place it as:
--
-- > foo :: Int -> String -> CHP ()
-- > foo = process "foo" $ \n s -> ...
--
-- Or as follows:
--
-- > foo :: Int -> String -> CHP ()
-- > foo = process "foo" foo'
-- >   where
-- >     foo' n s = ...
--
-- The annotation must capture all the parameters to the process.  What you must /not/ do is place it
-- such that there are free parameters not captured, for example this is /wrong/:
--
-- > foo :: Int -> String -> CHP ()
-- > foo n s = process "foo" foo'
-- >   where
-- >     foo' = ...
--
-- If you do want to have recursive processes that have outer parameters and not
-- pass them, you must wrap the outer process in 'process' and the inner process(es)
-- in 'subProcess'.
process :: Process p => String -> p -> p
process s = process' True s []

subProcess :: Process p => String -> p -> p
subProcess s = process' False s []
