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
import qualified Data.IntMap.Strict as M

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

type CollisionDetector = Coord -> TowerMap -> Collision

data Collision = HitNothing
               | HitPlayer
               | HitBuilding Int Building
  deriving (Show, Eq)

-- TODO: I don't think that I've accounted for falling off of either edge (!!!)
-- i.e. < 0 and > 128

-- The rules for wrapping are tricky here.  Rely on the play through
-- log functionality to test them.
findRightOf :: CollisionDetector
findRightOf coord towerMap' =
  let x'             = getX coord
      startingCoord  = (coord + missileSpeed)
      playerHitCheck =
        if coord < 0 || x' >= (width - missileSpeed)
        then HitPlayer
        else HitNothing
  in case M.lookupLT startingCoord towerMap' of
       Nothing                    -> playerHitCheck
       Just (coordHit, building') ->
         if coordHit >= coord
         then HitBuilding coordHit building'
         else playerHitCheck

firstOutOfBoundsCoord :: Int
firstOutOfBoundsCoord = width * height

findLeftOf :: CollisionDetector
findLeftOf coord towerMap' =
  let x'             = getX coord
      startingCoord  = (coord - missileSpeed)
      playerHitCheck =
        if coord >= firstOutOfBoundsCoord || x' < missileSpeed
        then HitPlayer
        else HitNothing
  in case M.lookupGT startingCoord towerMap' of
       Nothing                    -> playerHitCheck
       Just (coordHit, building') ->
         if coordHit <= coord
         then HitBuilding coordHit building'
         else playerHitCheck

definedAt :: Coord -> TowerMap -> Bool
definedAt = M.member

removeAt :: Coord -> TowerMap -> TowerMap
removeAt = M.delete
