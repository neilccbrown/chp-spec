-- Communicating Haskell Processes.
-- Copyright (c) 2010, Neil Brown.
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
--  * Neither the name of Neil Brown nor the names of other
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
module Control.Concurrent.CHPSpec.Spec where

import Control.Compose (Flip(..))
import qualified Data.Foldable as F
import Data.List (nub)

-- Specification:
data SpecItem' proc comm
  = Par [Spec' proc comm]
  | Alt [Spec' proc comm]
  | Call proc
  | Sync comm
  | Stop
  | Repeat (Spec' proc comm)
  deriving (Show)

type Spec' proc comm = [SpecItem' proc comm]

data Dir = DirInput | DirOutput deriving (Eq, Read, Show)

type ProcessId = Integer

type EventId = Integer

type CommId = Either EventId (EventId, Dir, Integer)

type SpecItem = SpecItem' ProcessId CommId

type Spec = Spec' ProcessId CommId

type SpecMod = Spec -> Spec

finalise :: SpecMod -> Spec
finalise f = f []

-- Instances:

instance Functor (SpecItem' p) where
  fmap _ (Call x) = Call x
  fmap f (Sync x) = Sync $ f x
  fmap _ Stop = Stop
  fmap f (Repeat x) = Repeat $ fmap (fmap f) x
  fmap f (Alt x) = Alt $ fmap (fmap (fmap f)) x
  fmap f (Par x) = Par $ fmap (fmap (fmap f)) x

instance Functor (Flip SpecItem' c) where
  fmap f = Flip . fmap' . unFlip
    where
      fmap' (Call x) = Call $ f x
      fmap' (Sync x) = Sync x
      fmap' Stop = Stop
      fmap' (Repeat x) = Repeat $ fmap fmap' x
      fmap' (Alt x) = Alt $ fmap (fmap fmap') x
      fmap' (Par x) = Par $ fmap (fmap fmap') x

instance F.Foldable (SpecItem' p) where
  foldr f x (Par xs) = F.foldr (flip $ F.foldr (flip $ F.foldr f)) x xs
  foldr f x (Alt xs) = F.foldr (flip $ F.foldr (flip $ F.foldr f)) x xs
  foldr _ x (Call _) = x
  foldr _ x Stop = x
  foldr f x (Sync y) = f y x
  foldr f x (Repeat y) = F.foldr (flip $ F.foldr f) x y

instance F.Foldable (Flip SpecItem' c) where
  foldr = (\f x -> foldr' f x . unFlip)
    where
      foldr' f x (Par xs) = F.foldr (flip $ F.foldr (flip $ foldr' f)) x xs
      foldr' f x (Alt xs) = F.foldr (flip $ F.foldr (flip $ foldr' f)) x xs
      foldr' f x (Call y) = f y x
      foldr' _ x (Sync _) = x
      foldr' _ x Stop = x
      foldr' f x (Repeat y) = F.foldr (flip $ foldr' f) x y

instance (Eq a, Eq b) => Eq (SpecItem' a b) where
  (==) (Call x) (Call y) = x == y
  (==) (Sync x) (Sync y) = x == y
  (==) (Repeat x) (Repeat y) = x == y
  (==) (Par x) (Par y) = bagsEqual x y
  (==) (Alt x) (Alt y) = x == y
  (==) Stop Stop = True
  (==) _ _ = False

bagsEqual :: Eq a => [a] -> [a] -> Bool
bagsEqual [] [] = True
bagsEqual _ [] = False
bagsEqual [] _ = False
bagsEqual (x:xs) ys
  | 1 + length xs /= length ys = False
  | otherwise
  = case span (/= x) ys of
      (_, []) -> False
      (as, _:bs) -> bagsEqual xs (as ++ bs)

checkZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
checkZipWith f xs ys
  | length xs /= length ys = error "checkZipWith, wrong lengths"
  | otherwise = zipWith f xs ys

-- Removes single-item Pars, and removes duplicate Alt branches:
pruneSpec :: (Eq p, Eq c) => Spec' p c -> Spec' p c
pruneSpec = concatMap pruneSpecOne
  where
    pruneSpecOne :: (Eq p, Eq c) => SpecItem' p c -> Spec' p c
    pruneSpecOne (Par [x]) = pruneSpec x
    pruneSpecOne (Par xs) = [Par $ map pruneSpec xs]
    pruneSpecOne (Alt xs) = [Alt $ nub $ map pruneSpec xs]
    pruneSpecOne (Repeat x) = [Repeat $ pruneSpec x]
    pruneSpecOne x = [x]

-- First map is process names, second map is channel names
subSpec :: (p -> p') -> (c -> c') -> SpecItem' p c -> SpecItem' p' c'
subSpec fp fc = unFlip . fmap fp . Flip . fmap fc
