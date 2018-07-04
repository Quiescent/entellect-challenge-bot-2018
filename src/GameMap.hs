module GameMap (mapFold,
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
                    Row,
                    TowerMap)
import Magic
import qualified Data.IntMap as M

mapFold :: (Row -> a -> a) -> a -> TowerMap -> a
mapFold = M.foldr

mapFoldIndexed :: (Int -> Row -> a -> a) -> a -> TowerMap -> a
mapFoldIndexed = M.foldrWithKey

adjustAt :: (Building -> Building) -> (Int, Int) -> TowerMap -> TowerMap
adjustAt f (x, y) = M.adjust (M.adjust f x) y

replaceAt :: Building -> (Int, Int) -> TowerMap -> TowerMap
replaceAt building' = adjustAt (\ _ -> building')

addAt :: (Int, Int) -> Building -> TowerMap -> TowerMap
addAt (x, y) building' towerMap' =
  if not $ M.member y towerMap'
  then M.insert y (M.fromList [(x, building')]) towerMap'
  else M.adjust (M.insert x building') y towerMap'

type CollisionDetector = (Int, Int) -> TowerMap -> Collision

data Collision = HitNothing
               | HitPlayer
               | HitBuilding Int Building

findRightOf :: CollisionDetector
findRightOf (x', y') towerMap' =
  case towerMap' M.!? y' >>= (M.lookupLE (x' + 2)) of
    Nothing                ->
      if x' == -1
      then HitPlayer
      else HitNothing
    Just (xHit, building') ->
      if xHit > x'
      then HitBuilding xHit building'
      else HitNothing

findLeftOf :: CollisionDetector
findLeftOf (x', y') towerMap' =
  case towerMap' M.!? y' >>= (M.lookupGE (x' - 2)) of
    Nothing                ->
      if x' == width
      then HitPlayer
      else HitNothing
    Just (xHit, building') ->
      if xHit < x'
      then HitBuilding xHit building'
      else HitNothing

definedAt :: (Int, Int) -> TowerMap -> Bool
definedAt (x', y') towerMap' =
  M.member y' towerMap' && M.member x' (towerMap' M.! y')

removeIfEmpty :: Int -> TowerMap -> TowerMap
removeIfEmpty y' towerMap' =
  case M.lookup y' towerMap' of
    Just x ->
      if x == M.empty
      then M.delete y' towerMap'
      else towerMap'
    _      -> towerMap'

removeAt :: (Int, Int) -> TowerMap -> TowerMap
removeAt (x', y') = removeIfEmpty y' . M.adjust (M.delete x') y'
