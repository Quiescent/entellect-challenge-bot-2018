module GameMap (rowAt,
                mapFold,
                removeAt,
                mapFoldIndexed,
                adjustAt,
                replaceAt,
                definedAt,
                addAt,
                findRightOf,
                findLeftOf,
                CollisionDetector,
                Collision(..))
  where

import Interpretor (Building(..),
                    TowerMap)
import Magic
import qualified Data.IntMap as M

import Coord

rowAt :: Int -> TowerMap -> TowerMap
rowAt y' towerMap' =
  fst $ M.split (toCoord width y') beforeTrimmed
  where
    beforeTrimmed = snd $ M.split ((toCoord 0 y') - 1) towerMap'

mapFold :: (Building -> a -> a) -> a -> TowerMap -> a
mapFold = M.foldr

mapFoldIndexed :: (Int -> Building -> a -> a) -> a -> TowerMap -> a
mapFoldIndexed = M.foldrWithKey

adjustAt :: (Building -> Building) -> Coord -> TowerMap -> TowerMap
adjustAt = M.adjust

replaceAt :: Building -> Coord -> TowerMap -> TowerMap
replaceAt building' = adjustAt (\ _ -> building')

addAt :: Coord -> Building -> TowerMap -> TowerMap
addAt coord building' towerMap' = M.insert coord building' towerMap'

type CollisionDetector = (Int, Int) -> TowerMap -> Collision

data Collision = HitNothing
               | HitPlayer
               | HitBuilding Int Building
  deriving (Show, Eq)

findRightOf :: CollisionDetector
findRightOf (x', y') towerMap' =
  case M.lookupLT (toCoord (x' + 2) y') towerMap' of
    Nothing                ->
      if x' <= -1
      then HitPlayer
      else HitNothing
    Just (coord, building') ->
      let (xHit, _) = fromCoord coord
      in if xHit >= x'
         then HitBuilding xHit building'
         else if x' <= -1
              then HitPlayer
              else HitNothing

findLeftOf :: CollisionDetector
findLeftOf (x', y') towerMap' =
  case M.lookupGT (toCoord (x' - 2) y') towerMap' of
    Nothing                ->
      if x' >= width
      then HitPlayer
      else HitNothing
    Just (coord, building') ->
      let (xHit, _) = fromCoord coord
      in if xHit <= x'
         then HitBuilding xHit building'
         else if x' >= width
              then HitPlayer
              else HitNothing

definedAt :: Coord -> TowerMap -> Bool
definedAt = M.member

removeAt :: Coord -> TowerMap -> TowerMap
removeAt = M.delete
