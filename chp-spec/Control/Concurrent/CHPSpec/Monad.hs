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

-- | This module contains all the central monads in the CHP library.
module Control.Concurrent.CHPSpec.Monad
  (
   -- * CHP Monad
  CHP, MonadCHP(..), liftIO_CHP, liftIO_CHP', foreverP, specify, Process, process, subProcess,

  onPoisonTrap, onPoisonRethrow, throwPoison, Poisonable(..), poisonAll,

  -- * Primitive actions
  skip, stop --, waitFor
   ) where

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F (toList)
import Data.Function (on)
import Data.List (intercalate, nubBy)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified Text.PrettyPrint.HughesPJ as PP

-- This module primarily re-exports the public definitions from
-- Control.Concurrent.CHP.{Base,CSP,Poison}:

import Control.Concurrent.CHPSpec.Base
import Control.Concurrent.CHPSpec.CSP
import Control.Concurrent.CHPSpec.LazySmallCheck
import Control.Concurrent.CHPSpec.Print
import Control.Concurrent.CHPSpec.Process
import Control.Concurrent.CHPSpec.Spec

-- | Models processes that run forever.
--
-- Anything following a 'foreverP' call in sequence will not be modelled.
foreverP :: CHP a -> CHP b
foreverP = stopSpecT . liftM (Repeat . snd) . finSpecT

skip :: CHP ()
skip = return ()

stop :: CHP a
stop = stopSpecT $ return Stop

-- | Like fromMaybe, but treats Just "" like Nothing
fromMaybeStr :: String -> Maybe String -> String
fromMaybeStr def m = case m of
  Just str | not (null str) -> str
  _ -> def

-- | Repeatedly applies the given function to the contents of the map until it
-- no longer changes
iter :: (Eq events, Ord k) => (Map.Map k events -> spec -> events) -> (Map.Map k (spec, events)) -> (Map.Map k (spec, events))
iter f m = let m' = Map.map ((id &&& f (Map.map snd m)) . fst) m
            in if Map.map snd m == Map.map snd m'
                 then m'
                 else iter f m'

-- | The top-level function in this library, to be used in place of runCHP in your
-- program.  You pass it a boolean (True if you want to leave the dummy IO events
-- exposed, False if you want them hidden) and a CHP process that you want to specify.
--  The result is a String containing a CSP-M specification that can be written
-- out to a file and read in to other tools, such as FDR, PRoBE and others.
specify :: Bool -> CHP () -> IO String
specify showIO main
  = specify' showIO <$> execStateT (finSpecT $ process "main" main) emptyCHPState
  where
    emptyCHPState = CHPState Map.empty Set.empty Map.empty [] 1 1
  
specify' :: Bool -> CHPState -> String
specify' showIO st = render specs ++ declMain
  where
    render :: Map.Map String [SpecItem' String String] -> String
    render = PP.render . PP.vcat . (map pprintForwardDecl allChans ++) . map (pprintProc eventMap) . Map.toList

    declMain :: String
    declMain = "\nmain = main_1 " ++ (if showIO then "" else
      "\\ {" ++ intercalate "," (map getName $ Set.toList $ chpIOEvents st) ++ "}"
      ) ++ "\n"
      where
        getName = fromJust . flip Map.lookup events

    pulledProcesses :: Map.Map (Integer, String) [SpecItem]
    pulledProcesses =
       collapseMapMap (\s n -> (n, s ++ "_" ++ show n))
                      (Map.unionWith Map.union oldProcs pulledProcs)
      where
        rawProcesses :: Map.Map String (Map.Map Integer Spec)
        rawProcesses = Map.map (Map.map (fst . snd)) $ chpProcessMap st

        rep :: Spec -> State (Integer, Map.Map String (Map.Map Integer Spec)) Integer
        rep x = do (!n, m) <- get
                   put (succ n, insertMapMap "repeated" n (x ++ [Call n]) m)
                   return n

        oldProcs, pulledProcs :: Map.Map String (Map.Map Integer Spec)
        (oldProcs, (_, pulledProcs))
          = runState (T.mapM (T.mapM $ T.mapM $ pullUpRepeatM rep) rawProcesses)
                     (chpNextProcess st, Map.empty)

    specs :: Map.Map String [SpecItem' String String]
    specs = Map.mapKeys snd $ Map.map (fmap (subSpec showProc showComm) . pruneSpec) pulledProcesses
      where
        showProc :: ProcessId -> String
        showProc p = fromMaybeStr ("p_" ++ show p) $ Map.lookup p procs

        showComm :: CommId -> String
        showComm (Left c) = fromMaybeStr ("c_" ++ show c) $ Map.lookup c events
        showComm (Right (c, d, x)) = showComm (Left c) ++ (if d == DirInput
          then "?" else "!") ++ "x_" ++ show x

        procs = Map.fromList $ Map.keys pulledProcesses

    allChans = nubBy ((==) `on` baseName) $ F.toList $ foldr Set.union Set.empty $ Map.elems eventMap

    events = chpEventMap st

    eventMap = Map.map snd $ iter (findAllComms baseName) $ Map.map (flip (,) Set.empty) $ specs

    baseName = takeWhile (`notElem` "?!.")

-- | Models the lifting of an IO action into the CHP monad.
--
-- The IO computation itself is completely ignored.  The label (first parameter)
-- is used to label various different dummy events, which arise from exploring
-- the return type of the IO computation.  To support this exploration, the return
-- type must be an instance of 'Serial'.
--
-- More details and a full explanation of how IO events are modelled are available
-- in this blog post: <http://chplib.wordpress.com/2010/04/20/automatic-model-generation-part-3-choice-and-io/>
liftIO_CHP' :: Serial a => String -> IO a -> CHP a
liftIO_CHP' label _ = CHPSpecT $ \k ->
    do (vals, complete) <- fuzz k
       unless complete $
         liftIO $ putStrLn "Incomplete fuzzing of IO computation"
       nonces <- replicateM (length vals) newEvent
       let firstLabel = "IO_" ++ label ++ show (head nonces)
       zipWithM_ labelEvent nonces (map (firstLabel++) suffixes)
       modify $ \st -> st { chpIOEvents = Set.union
                              (Set.fromList nonces) (chpIOEvents st) }
       return (error "liftIO return", \s -> [Alt $ zipWith (\n f -> Sync (Left n) : snd f s) nonces vals])
  where
    suffixes = map (:[]) ['A'..'Z'] ++ map show [(0::Integer)..]

-- | Like @liftIO_CHP'@, but with an empty label.
liftIO_CHP :: Serial a => IO a -> CHP a
liftIO_CHP = liftIO_CHP' ""
