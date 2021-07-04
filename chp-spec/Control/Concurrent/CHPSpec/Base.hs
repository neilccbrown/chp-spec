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



-- | A module containing various definitions relating to the CSP\/CSPPoison
-- monads, and poison.  Not publicly visible.
module Control.Concurrent.CHPSpec.Base where

import Control.Arrow
import Control.Monad (liftM)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans(..))
import Data.Dynamic
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Concurrent.CHPSpec.Spec

newtype CHPSpecT m a = CHPSpecT {runSpecT :: forall b. (a -> m (b, SpecMod)) -> m (b, SpecMod) }

instance MonadTrans CHPSpecT where
  lift m = CHPSpecT (m >>=)

addSpecT1 :: forall m a. Monad m => m (a, SpecItem) -> CHPSpecT m a
addSpecT1 m = CHPSpecT $ \k -> m >>= apply k
  where
    apply :: (a -> m (b, SpecMod)) -> (a, SpecItem) -> m (b, SpecMod)
    apply k (x, s) = liftM (second ((s :) .)) $ k x

finSpecT :: Monad m => CHPSpecT m a -> m (a, Spec)
finSpecT = liftM (second finalise) . flip runSpecT (\x -> return (x, id))

stopSpecT :: Monad m => m SpecItem -> CHPSpecT m a
stopSpecT m = CHPSpecT $ const $ liftM (\sp -> (error "stopSpecT", (sp :))) m

instance Monad m => Monad (CHPSpecT m) where
  return x = CHPSpecT ($ x)
  m >>= k  = CHPSpecT $ \c -> runSpecT m $ \a -> runSpecT (k a) c

newtype Enrolled b a = Enrolled (b a) deriving (Eq)

instance Typeable (b a) => Typeable (Enrolled b a) where
  typeOf x = let Enrolled b = Enrolled undefined `asTypeOf` x in mkTyConApp (mkTyCon "Enrolled") [typeOf b]

type CheckArg = Dynamic -> Bool

data CHPState = CHPState
  { chpEventMap :: Map.Map Integer String
  , chpIOEvents :: Set.Set Integer
  , chpProcessMap :: Map.Map String (Map.Map Integer ([CheckArg], (Spec, Dynamic)))
  , chpFreeNames :: [(Dynamic, CheckArg)]
  , chpNextBottom :: !Integer
  , chpNextProcess :: !Integer
  }

-- | The central monad of the library.  You can use
-- the 'specify' function to model programs written with this monad.
type CHP = CHPSpecT (StateT CHPState IO)

class Monad m => MonadCHP m where
  liftCHP :: CHP a -> m a

class Poisonable c where
  poison :: MonadCHP m => c -> m ()
  checkForPoison :: MonadCHP m => c -> m ()

-- | Throws a poison exception.  Poison is not currently modelled.
throwPoison :: CHP a
throwPoison = return $ error "throwPoison" -- checkPoison PoisonItem

-- | Allows you to provide a handler for sections with poison.  Since poison is
-- not currently modelled, this acts like 'const' at the moment.
onPoisonTrap :: forall a. CHP a -> CHP a -> CHP a
onPoisonTrap x _ = x

-- | Like 'onPoisonTrap', this function allows you to provide a handler for
--  poison.  Since poison is not currently modelled, this acts like 'const' at
--  the moment.
onPoisonRethrow :: CHP a -> CHP () -> CHP a
onPoisonRethrow x _ = x

poisonAll :: (Poisonable c, MonadCHP m) => [c] -> m ()
poisonAll = mapM_ poison

instance MonadCHP CHP where
  liftCHP = id

altSpecT :: Monad m => [CHPSpecT m a] -> CHPSpecT m a
altSpecT ms = CHPSpecT $
  \k -> do xfs <- mapM (flip runSpecT k) ms
           return (error "alt return", \s -> [Alt $ map (($ s) . snd) xfs])


insertMapMap :: (Ord k, Ord k') => k -> k' -> v -> Map.Map k (Map.Map k' v) -> Map.Map k (Map.Map k' v)
insertMapMap k k' v = Map.insertWith Map.union k (Map.singleton k' v)

collapseMapMap :: Ord kk => (k -> k' -> kk) -> Map.Map k (Map.Map k' v) -> Map.Map kk v
collapseMapMap f = Map.fromList . concatMap (\(k, kvs) -> map (first $ f k) kvs) . Map.toList . Map.map Map.toList
