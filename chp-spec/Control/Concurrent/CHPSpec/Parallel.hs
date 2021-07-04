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

-- | A module for running items in parallel.  See <http://chplib.wordpress.com/2010/04/13/automatic-model-generation-part-1-parallel/>
-- for details of how parallel items are modelled.
--
-- Parallel specifications are well supported, and the only change to this module
-- from normal CHP is that forking is not currently supported.
module Control.Concurrent.CHPSpec.Parallel (runParallel, runParallel_, (<||>), (<|*|>),
  runParMapM, runParMapM_) where

import Control.Arrow
import Control.Monad.Reader

import Control.Concurrent.CHPSpec.Base
import Control.Concurrent.CHPSpec.Spec

runParallel :: [CHP a] -> CHP [a]
runParallel = addSpecT1 . liftM (second Par . unzip) . mapM finSpecT

runParMapM :: (a -> CHP b) -> [a] -> CHP [b]
runParMapM f = runParallel . map f

runParMapM_ :: (a -> CHP b) -> [a] -> CHP ()
runParMapM_ f = runParallel_ . map f

(<||>) :: CHP a -> CHP b -> CHP (a, b)
(<||>) p q = do [x, y] <- runParallel [liftM Left p, liftM Right q]
                combine x y
    where
      combine :: Monad m => Either a b -> Either a b -> m (a, b)
      combine (Left x) (Right y) = return (x, y)
      combine (Right y) (Left x) = return (x, y)
      -- An extra case to keep the compiler happy:
      combine _ _ = error "Impossible combination values in <|^|>"

(<|*|>) :: CHP a -> CHP b -> CHP ()
(<|*|>) p q = runParallel_ [p >> return (), q >> return ()]

runParallel_ :: [CHP a] -> CHP ()
runParallel_ procs = runParallel procs >> return ()

-- We right associate to allow the liftM fst ((readResult) <||> runParallel_
-- workers) pattern
infixr <||>
-- Doesn't really matter for this operator:
infixr <|*|>

