module My.Prelude
  ( module Codec.Archive.Tar.Entry,
    module Control.Applicative,
    module Control.DeepSeq,
    module Control.Monad,
    module Control.Monad.Except,
    module Control.Monad.Extra,
    module Control.Monad.Loops,
    module Data.Bifunctor,
    module Data.Bool,
    module Data.Char,
    module Data.Data,
    module Data.Either,
    module Data.Eq,
    module Data.Fixed,
    module Data.Foldable,
    module Data.Function,
    module Data.Functor,
    module Data.Int,
    module Data.List,
    module Data.List.NonEmpty,
    module Data.Map,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Ord,
    module Data.Semigroup,
    module Data.Sequence,
    module Data.Set,
    module Data.Text,
    module Data.Text.Encoding,
    module Data.Time.Clock.System,
    module Data.Traversable,
    module Data.Tuple,
    module Data.Tuple.Extra,
    module Foreign,
    module GHC.Enum,
    module GHC.Float,
    module GHC.Generics,
    module GHC.Integer,
    module GHC.Num,
    module GHC.Real,
    module GHC.Show,
    module Protolude,
    module My.Prelude,
    module Safe,
    module Safe.Foldable,
    module Universum,
    module System.Random,
    FilePath,
  )
where

import Codec.Archive.Tar.Entry (getDirectoryContentsRecursive) -- is there a more appropriate recursive directory listing functon?
import Control.Applicative ((<*>), (<|>), Alternative, Applicative, ZipList (ZipList, getZipList), pure)
import Control.DeepSeq (NFData)
import Control.Monad ((<=<), (=<<), (>>), (>>=), Monad, fail, filterM, foldM, forever, join, mfilter, return, unless, void, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
-- UNSAFE, DO NOT IMPORT: foldl1, foldr1
-- mod' is incorrect for large Doubles and always returns 0.

import "monad-extras" Control.Monad.Extra (unfoldM_)
import "extra" Control.Monad.Extra (ifM, unlessM, whenM, whileM)
import Control.Monad.Loops (iterateM_)
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Bool ((&&), Bool (False, True), not, otherwise, (||))
import Data.Char (Char)
import Data.Data (Data, toConstr)
import Data.Either (Either (Left, Right), either, isLeft, isRight)
import Data.Eq (Eq ((/=), (==)))
import Data.Fixed (mod')
import Data.Foldable (Foldable, all, any, elem, find, fold, foldl, foldlM, foldr, foldrM, forM_, for_, length, mapM_, null, sum, toList, traverse_)
import Data.Function (($), (&), (.), flip, id)
import Data.Functor (($>), (<$), (<$>), (<&>), Functor, fmap)
import Data.Int (Int)
import Data.List ((\\), concat, drop, dropWhile, filter, iterate, nub, repeat, replicate, reverse, sort, sortBy, sortOn, take, takeWhile, unzip)
import qualified Data.List
import qualified Data.List.NonEmpty
import Data.List.NonEmpty (NonEmpty ((:|)), groupAllWith, groupBy, groupWith, head, last, unfoldr)
import Data.Map (Map, keys, mapKeys)
import qualified Data.Map
import Data.Maybe (Maybe (Just, Nothing), catMaybes, fromMaybe, isJust, isNothing, maybe)
import Data.Monoid ((<>), Any, Monoid, mempty)
import Data.Ord (Ord ((<), (<=), (>), (>=)), max, min)
import Data.Semigroup (Semigroup)
import Data.Sequence (iterateN)
import Data.Set (Set, difference, map, union)
import qualified Data.Set
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock.System (SystemTime)
import Data.Traversable (for, forM, sequence, traverse)
import Data.Tuple (fst, snd, swap, uncurry)
import Data.Tuple.Extra (dupe)
import Foreign (ForeignPtr)
import GHC.Enum (Bounded, Enum, enumFrom, maxBound, minBound)
import GHC.Float ((**), Double, Float, divideDouble)
import GHC.Generics (Generic)
import GHC.Integer (Integer)
import GHC.Num ((*), (+), (-), Num, abs, fromInteger, subtract)
import GHC.Real ((/), Fractional, Integral, fromIntegral)
import GHC.Show (Show (show))
import Protolude ((<<$>>), (<<*>>))
import Safe (headMay, lastMay)
import Safe.Foldable (foldl1Safe, foldr1Safe)
import System.IO (FilePath)
import System.Random (StdGen, mkStdGen, random, randomR)
import Universum (foldl1, foldr1)

mapDelete :: Ord k => k -> Map k a -> Map k a
mapDelete = Data.Map.delete

mapFromList :: Ord k => [(k, a)] -> Map k a
mapFromList = Data.Map.fromList

mapInsert :: Ord k => k -> a -> Map k a -> Map k a
mapInsert = Data.Map.insert

mapLookup :: Ord k => k -> Map k a -> Maybe a
mapLookup = Data.Map.lookup

mapNull :: Map k a -> Bool
mapNull = Data.Map.null

mapToList :: Map k a -> [(k, a)]
mapToList = Data.Map.toList

mapSingleton :: k -> a -> Map k a
mapSingleton = Data.Map.singleton

mapUnion :: Ord k => Map k a -> Map k a -> Map k a
mapUnion = Data.Map.union

setDelete :: Ord a => a -> Set a -> Set a
setDelete = Data.Set.delete

setFilter :: (a -> Bool) -> Set a -> Set a
setFilter = Data.Set.filter

setFromList :: Ord a => [a] -> Set a
setFromList = Data.Set.fromList

setInsert :: Ord a => a -> Set a -> Set a
setInsert = Data.Set.insert

setMember :: Ord a => a -> Set a -> Bool
setMember = Data.Set.member

setSingleton :: a -> Set a
setSingleton = Data.Set.singleton

nelTakeWhile :: (a -> Bool) -> NonEmpty a -> [a]
nelTakeWhile = Data.List.NonEmpty.takeWhile

listDelete :: Eq a => a -> [a] -> [a]
listDelete = Data.List.delete

listIsSuffixOf :: Eq a => [a] -> [a] -> Bool
listIsSuffixOf = Data.List.isSuffixOf

-- similar to both in lens
both :: Data.Bifunctor.Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f

-- similar to (??) in lens
(??) :: Functor f => f (a -> b) -> a -> f b
(??) ff x = (\f -> f x) <$> ff

-- named version of (??), name inspired by relude
flap :: Functor f => f (a -> b) -> a -> f b
flap = (??)
