-- | This module is a near-copy of the Test.LazySmallCheck module from the lazysmallcheck
-- package on Hackage.  It was written by Matthew Naylor and Fredrik Lindblad.
--  I have modified it to add the 'fuzz' function that is used by chp-spec internally,
-- but to write that function I needed access to more internals than the original
-- Test.LazySmallCheck exposed, hence I had to make a copy of the module.  Unfortunately,
-- this means that the Serial type-class here is a different type-class (despite
-- being identical in behaviour and API) from the original lazysmallcheck package.
-- The modified module is exposed in case you need to supply any of your own instances
-- for 'Serial'.  For more explanation of how Lazy SmallCheck is used in this library
-- for modelling IO actions, see this blog post: <http://chplib.wordpress.com/2010/04/20/automatic-model-generation-part-3-choice-and-io/>
module Control.Concurrent.CHPSpec.LazySmallCheck
  ( Serial(series) -- :: class
  , Series         -- :: type Series a = Int -> Cons a
  , Cons           -- :: *
  , cons           -- :: a -> Series a
  , (><)           -- :: Series (a -> b) -> Series a -> Series b
  , (\/)           -- :: Series a -> Series a -> Series a
  , drawnFrom      -- :: [a] -> Cons a
  , cons0          -- :: a -> Series a
  , cons1          -- :: Serial a => (a -> b) -> Series b
  , cons2          -- :: (Serial a, Serial b) => (a -> b -> c) -> Series c
  , cons3          -- :: ...
  , cons4          -- :: ...
  , cons5          -- :: ...
  , fuzz
  )
  where

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Exception
import Control.Monad.State

infixr 3 \/
infixl 4 ><

type Pos = [Int]

data Term = Var Pos Type | Ctr Int [Term]

data Type = SumOfProd [[Type]]

type Series a = Int -> Cons a

data Cons a = C Type ([[Term] -> a]) Bool

class Serial a where
  series :: Series a

-- Series constructors

cons :: a -> Series a
cons a d = C (SumOfProd [[]]) [const a] True

(><) :: Series (a -> b) -> Series a -> Series b
(f >< a) d = C (SumOfProd [ta:p | shallow, p <- ps]) cs (shallow && fComplete && aComplete)
  where
    C (SumOfProd ps) cfs fComplete = f d
    C ta cas aComplete = a (d-1)
    cs = [\(x:xs) -> cf xs (conv cas x) | shallow, cf <- cfs]
    shallow = d > 0 && nonEmpty ta

nonEmpty :: Type -> Bool
nonEmpty (SumOfProd ps) = not (null ps)

(\/) :: Series a -> Series a -> Series a
(a \/ b) d = C (SumOfProd (ssa ++ ssb)) (ca ++ cb) (aComplete && bComplete)
  where
    C (SumOfProd ssa) ca aComplete = a d
    C (SumOfProd ssb) cb bComplete = b d

conv :: [[Term] -> a] -> Term -> a
conv cs (Var p _) = error ('\0':map toEnum p)
conv cs (Ctr i xs) = (cs !! i) xs

drawnFrom :: [a] -> Cons a
drawnFrom xs = C (SumOfProd (map (const []) xs)) (map const xs) False

-- Helpers, a la SmallCheck

cons0 :: a -> Series a
cons0 f = cons f

cons1 :: Serial a => (a -> b) -> Series b
cons1 f = cons f >< series

cons2 :: (Serial a, Serial b) => (a -> b -> c) -> Series c
cons2 f = cons f >< series >< series

cons3 :: (Serial a, Serial b, Serial c) => (a -> b -> c -> d) -> Series d
cons3 f = cons f >< series >< series >< series

cons4 :: (Serial a, Serial b, Serial c, Serial d) =>
  (a -> b -> c -> d -> e) -> Series e
cons4 f = cons f >< series >< series >< series >< series

cons5 :: (Serial a, Serial b, Serial c, Serial d, Serial e) =>
  (a -> b -> c -> d -> e -> f) -> Series f
cons5 f = cons f >< series >< series >< series >< series >< series

-- Standard instances

instance Serial () where
  series = cons0 ()

instance Serial Bool where
  series = cons0 False \/ cons0 True

instance Serial a => Serial (Maybe a) where
  series = cons0 Nothing \/ cons1 Just

instance (Serial a, Serial b) => Serial (Either a b) where
  series = cons1 Left \/ cons1 Right

instance Serial a => Serial [a] where
  series = cons0 [] \/ cons2 (:)

instance (Serial a, Serial b) => Serial (a, b) where
  series = cons2 (,) . (+1)

instance (Serial a, Serial b, Serial c) => Serial (a, b, c) where
  series = cons3 (,,) . (+1)

instance (Serial a, Serial b, Serial c, Serial d) =>
    Serial (a, b, c, d) where
  series = cons4 (,,,) . (+1)

instance (Serial a, Serial b, Serial c, Serial d, Serial e) =>
    Serial (a, b, c, d, e) where
  series = cons5 (,,,,) . (+1)

instance Serial Int where
  series d = drawnFrom [-d..d]

instance Serial Integer where
  series d = drawnFrom (map toInteger [-d..d])

instance Serial Char where
  series d = drawnFrom (take (d+1) ['a'..])

instance Serial Float where
  series d = drawnFrom (floats d)

instance Serial Double where
  series d = drawnFrom (floats d)

floats :: RealFloat a => Int -> [a]
floats d = [ encodeFloat sig exp
           | sig <- map toInteger [-d..d]
           , exp <- [-d..d]
           , odd sig || sig == 0 && exp == 0
           ]

-- Term refinement

refine :: Term -> Pos -> [Term]
refine (Var p (SumOfProd ss)) [] = new p ss
refine (Ctr c xs) p = map (Ctr c) (refineList xs p)

refineList :: [Term] -> Pos -> [[Term]]
refineList xs (i:is) = [ls ++ y:rs | y <- refine x is]
  where (ls, x:rs) = splitAt i xs

new :: Pos -> [[Type]] -> [Term]
new p ps = [ Ctr c (zipWith (\i t -> Var (p++[i]) t) [0..] ts)
           | (c, ts) <- zip [0..] ps ]

-- Find total instantiations of a partial value

total :: Term -> [Term] 
total val = tot val
  where
    tot (Ctr c xs) = [Ctr c ys | ys <- mapM tot xs] 
    tot (Var p (SumOfProd ss)) = [y | x <- new p ss, y <- tot x]

-- Answers

answer :: IO a -> (a -> IO b) -> (Pos -> IO b) -> IO b
answer a known unknown =
  do res <- try a
     case res of
       Right b -> known b
       Left (ErrorCall ('\0':p)) -> unknown (map fromEnum p)
       Left e -> throw e

resultVal :: forall b s. Result s b -> StateT s IO ([b], Bool)
resultVal r = ref (args r)
  where
    ref :: [Term] -> StateT s IO ([b], Bool)
    ref xs = StateT $ \s -> answer (flip runStateT s $ apply r xs) (\(x, s) -> return (([x], complete r), s)) (unknown s)
      where
        unknown :: s -> Pos -> IO (([b], Bool), s)
        unknown s p = (first $ (concat *** and) . unzip) <$> (flip runStateT s $ mapM ref (refineList xs p))

data Result s b =
  Result { args     :: [Term]
         , apply    :: [Term] -> StateT s IO b
         , complete :: Bool
         }

--data P b = P (Int -> Int -> Result b)

runEnd :: ([Term] -> StateT s IO r) -> Int -> Int -> Result s r
runEnd f _ _ = Result [] (f . reverse) True

runParam :: Serial a => ([Term] -> a -> StateT s IO r) -> Int -> Int -> Result s r
runParam f n d =
    let C t c comp = series d
        c' = conv c
        r = runEnd (\(x:xs) -> f xs (c' x)) (n+1) d
    in  r { args = Var [n] t : args r, complete = complete r && comp }

-- Top-level interface

depthCheck :: Serial a => Int -> (a -> StateT s IO r) -> StateT s IO ([r], Bool)
depthCheck d p = resultVal $ runParam (const p) 0 d

smallCheck :: Serial a => Int -> (a -> StateT s IO r) -> StateT s IO ([r], Bool)
smallCheck d p = smallCheck' [] 0
  where
    -- Should I accumulate prev?  I think later depths subsume earlier ones...
    smallCheck' prev n
      | n <= d = do (xs, comp) <- depthCheck n p
                    if comp
                      then return (xs, True)
                      else smallCheck' xs (succ n)
      | otherwise = return (prev, False)

--test :: Testable a r => a -> IO ()
--test p = mapM_ (`depthCheck` p) [0..]

-- Finds all possible behaviours of the code block:
fuzz :: (Serial a) => (a -> StateT s IO b) -> StateT s IO ([b], Bool)
fuzz = smallCheck 10

