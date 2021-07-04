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

-- | A module with support for things that are enrollable.
--
-- Enrollment is currently pretty much ignored during model generation, but these
-- operations are provided so that you don't have to change your program.
module Control.Concurrent.CHPSpec.Enroll (Enrolled, Enrollable(..), furtherEnroll,
  enrollPair, enrollList, enrollAll, enrollAll_, enrollAllT, enrollOneMany) where

import Control.Concurrent.CHPSpec.Base
import Control.Concurrent.CHPSpec.Parallel

class Enrollable b z where
  enroll :: b z -> (Enrolled b z -> CHP a) -> CHP a
  resign :: Enrolled b z -> CHP a -> CHP a

furtherEnroll :: Enrollable b z => Enrolled b z -> (Enrolled b z -> CHP a) -> CHP a
furtherEnroll (Enrolled x) = enroll x

enrollPair :: (Enrollable b p, Enrollable b' p') => (b p, b' p') -> ((Enrolled
  b p, Enrolled b' p') -> CHP a) -> CHP a
enrollPair (b0,b1) f = enroll b0 $ \eb0 -> enroll b1 $ \eb1 -> f (eb0, eb1)

enrollList :: Enrollable b p => [b p] -> ([Enrolled b p] -> CHP a) -> CHP a
enrollList [] f = f []
enrollList (b:bs) f = enroll b $ \eb -> enrollList bs $ f . (eb:)

enrollAll :: Enrollable b p => CHP (b p) -> [Enrolled b p -> CHP a] -> CHP [a]
enrollAll = enrollAllT runParallel

enrollAllT :: Enrollable b p => ([a] -> CHP c) -> CHP (b p) -> [Enrolled b p -> a] -> CHP c
enrollAllT run mbar ps = mbar >>= flip enrollList (run . zipWith ($) ps) . replicate (length ps)

enrollOneMany :: Enrollable b p => ([Enrolled b p] -> CHP a) -> [(CHP (b p), Enrolled b p -> CHP c)] -> CHP (a, [c])
enrollOneMany p [] = p [] >>= \x -> return (x, [])
enrollOneMany p ((mbar, q) : rest)
  = do bar <- mbar
       enrollPair (bar, bar) $ \(b0, b1) ->
         do ((p', q'), rest') <- enrollOneMany (\bs -> p (b0:bs) <||> q b1) rest
            return (p', q' : rest')

enrollAll_ :: Enrollable b p => CHP (b p) -> [Enrolled b p -> CHP a] -> CHP ()
enrollAll_ = enrollAllT runParallel_
