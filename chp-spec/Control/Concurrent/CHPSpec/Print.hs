module Control.Concurrent.CHPSpec.Print where

import Control.Applicative
import Control.Arrow
import Control.Compose (Flip(..))
import qualified Data.Foldable as F
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified Text.PrettyPrint.HughesPJ as PP

import Control.Concurrent.CHPSpec.Spec

name :: String -> PP.Doc
name s = PP.text [if c `elem` valid then c else '_' | c <- s]
  where
    valid = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "?!."

pprintForwardDecl :: String -> PP.Doc
pprintForwardDecl n = PP.sep [PP.text "channel", name $ eventName n]

type ProcEvents = Map.Map String (Set.Set String)

pprintProc :: ProcEvents -> (String, Spec' String String) -> PP.Doc
pprintProc m (n, spec)
  = PP.hang (PP.hcat [name n, PP.text "="])
      2 (pprintCSP_Seq m spec)

pprintCSP_Seq :: ProcEvents -> Spec' String String -> PP.Doc
pprintCSP_Seq m = PP.parens . PP.sep . p
  where
    p [] = [PP.text "SKIP"]
    p (Sync c : xs) = [name c, PP.text "->"] ++ p xs
    p [x] = [pprintCSP m x]
    p (x:xs) = [pprintCSP m x, PP.text ";"] ++ p xs

pprintCSP :: ProcEvents -> SpecItem' String String -> PP.Doc
pprintCSP m (Alt s) = zeroOneMore m "STOP" (withOp m "[]") s
pprintCSP m (Par xs) = zeroOneMore m "SKIP" joinPar xs
  where
    joinPar :: [Spec' String String] -> PP.Doc
    joinPar [a] = pprintCSP_Seq m a
    joinPar (a:bs) = PP.parens $ PP.sep $ 
      [ pprintCSP_Seq m a
      , pprintParOp (findAllComms eventName m a `Set.intersection`
                       unionAll (map (findAllComms eventName m) bs))
      , joinPar bs
      ]
    joinPar _ = error "Impossible code reached in pprintCSP"
pprintCSP _ (Call p) = name p
pprintCSP _ x = error $ "pprintCSP: " ++ show x

withOp :: ProcEvents -> String -> [Spec' String String] -> PP.Doc
withOp m s = PP.parens . PP.sep . intersperse (PP.text s) . map (pprintCSP_Seq m)

zeroOneMore :: ProcEvents -> String ->
  ([Spec' String String] -> PP.Doc) -> [Spec' String String] -> PP.Doc
zeroOneMore _ zeroText _ [] = PP.text zeroText
zeroOneMore m _ _ [x] = pprintCSP_Seq m x
zeroOneMore _ _ f xs = f xs

eventName :: String -> String
eventName = takeWhile (`notElem` "?!.")

pprintParOp :: Set.Set String -> PP.Doc
pprintParOp s
  | Set.null s = PP.text "|||"
  | otherwise = surround "[|{|" "|}|]" $ intersperse PP.comma $ map name (Set.toList s)
  where
    surround a b x = PP.sep $ [PP.text a] ++ x ++ [PP.text b]

findAllComms :: forall f p c c'. (Show p, Ord p, Ord c', Functor f, F.Foldable f) =>
  (c -> c') -> Map.Map p (Set.Set c') -> f (SpecItem' p c) -> Set.Set c'
findAllComms f m = uncurry Set.union . (actualComms &&& lookupComms)
  where
    actualComms, lookupComms :: f (SpecItem' p c) -> Set.Set c'
    actualComms = unionAll . fmap (unionAll . fmap (Set.singleton . f))
    lookupComms = unionAll . fmap (unionAll . fmap (\n -> fromMaybe (error $ "findAllComms " ++ show (n, Map.keys m)) $ Map.lookup n m) . Flip)

unionAll :: (F.Foldable f, Ord a) => f (Set.Set a) -> Set.Set a
unionAll = F.foldr Set.union Set.empty


pullUpRepeatM :: (Functor m, Monad m) => (Spec' proc comm -> m proc) -> SpecItem' proc comm -> m (SpecItem' proc comm)
pullUpRepeatM f (Par xs) = Par <$> T.mapM (T.mapM $ pullUpRepeatM f) xs
pullUpRepeatM f (Alt xs) = Alt <$> T.mapM (T.mapM $ pullUpRepeatM f) xs
pullUpRepeatM f (Repeat x) = Call <$> f x
pullUpRepeatM _ x = return x
