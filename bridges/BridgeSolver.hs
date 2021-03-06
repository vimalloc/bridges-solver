module BridgeSolver (createGame, getXMax, getYMax, solve, toString) where

import Data.List (find, nub)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Data.Foldable (asum)
import Control.Applicative ((<|>), liftA2)
import Control.Exception.Base (assert)
import qualified Data.Map.Strict as Map


-- Left' and Right' use the tick so they don't clash with Eithers Left or Right.
-- Up' and Down' use the ticks to stay consistant with Left' and Right'.
data BridgeDirection = Up'
                     | Down'
                     | Left'
                     | Right' deriving (Eq, Show, Ord)


data BridgeValue = Single
                 | Double deriving (Eq, Show, Ord)


data Bridge = Bridge
    { getBridgeDirection :: !BridgeDirection
    , getBridgeValue     :: !BridgeValue
    } deriving (Eq, Show, Ord)


data Point = Point
    { getX :: !Int
    , getY :: !Int
    } deriving (Eq, Show, Ord)


data Island = Island
    { getIslandValue   :: !Int
    , getIslandBridges :: ![Bridge]
    } deriving (Eq, Show, Ord)


data Game = Game
    { getXMax    :: !Int
    , getYMax    :: !Int
    , getIslandPointMap :: !(Map.Map Point Island)
    } deriving (Eq, Show)


whenBool :: Bool -> a -> Maybe a
whenBool False _ = Nothing
whenBool True a  = Just a


verifyIslandValue :: Int -> Either String Int
verifyIslandValue i
    | i >= 1 && i <= 8 = Right i
    | otherwise        = Left "Island values must be between 1 and 8 inclusive"


bridgeToInt :: Bridge -> Int
bridgeToInt (Bridge _ Single) = 1
bridgeToInt (Bridge _ Double) = 2


numBridges :: Island -> Int
numBridges = sum . map (bridgeToInt) . getIslandBridges


islandOverFilled :: Island -> Bool
islandOverFilled i = numBridges i > getIslandValue i


islandFilled :: Island -> Bool
islandFilled i = numBridges i == getIslandValue i


isIsland :: Point -> Game -> Bool
isIsland p g = p `Map.member` (getIslandPointMap g)


isNotIsland :: Point -> Game -> Bool
isNotIsland p g = not $ isIsland p g


-- Terminal codes to make result blue and bold
islandToString :: Island -> String
islandToString i =  "\ESC[94m\ESC[1m" ++ show (getIslandValue i) ++ "\ESC[0m"


bridgeToString :: Bridge -> String
bridgeToString (Bridge Up' Single)    = "|"
bridgeToString (Bridge Down' Single)  = "|"
bridgeToString (Bridge Up' Double)    = "‖"
bridgeToString (Bridge Down' Double)  = "‖"
bridgeToString (Bridge Left' Single)  = "―"
bridgeToString (Bridge Right' Single) = "―"
bridgeToString (Bridge Left' Double)  = "═"
bridgeToString (Bridge Right' Double) = "═"


reverseBridge :: Bridge -> Bridge
reverseBridge (Bridge Up' v)    = Bridge Down' v
reverseBridge (Bridge Down' v)  = Bridge Up' v
reverseBridge (Bridge Left' v)  = Bridge Right' v
reverseBridge (Bridge Right' v) = Bridge Left' v


getIslands :: Game -> [Island]
getIslands = Map.elems . getIslandPointMap


getIslandPoints :: Game -> [Point]
getIslandPoints = Map.keys . getIslandPointMap


lookupIsland :: Point -> Game -> Maybe Island
lookupIsland p g = p `Map.lookup` (getIslandPointMap g)


getIsland :: Point -> Game -> Island
getIsland p g = fromJust $ lookupIsland p g


nextPoint :: BridgeDirection -> Point -> Point
nextPoint Up' (Point x y)    = (Point x (y-1))
nextPoint Down' (Point x y)  = (Point x (y+1))
nextPoint Left' (Point x y)  = (Point (x-1) y)
nextPoint Right' (Point x y) = (Point (x+1) y)


getBridgePoints :: Point -> BridgeDirection -> Game -> [Point]
getBridgePoints p d g = takeWhile (\p -> onBoard p && isNotIsland p g) allPoints
  where
    point      = assert (isJust $ lookupIsland p g) p
    startPoint = nextPoint d point
    allPoints  = iterate (nextPoint d) $ startPoint
    onBoard p  = (getX p <= getXMax g) && (getX p >= 0) &&
                 (getY p <= getYMax g) && (getY p >= 0)


lookupRemoteIslandPoint :: Point -> BridgeDirection -> Game -> Maybe Point
lookupRemoteIslandPoint p d g
    | null notOverlapping = Nothing
    | otherwise           = whenBool (isIsland islandPoint g) islandPoint
  where
    bridgePoints   = getBridgePoints p d g
    notOverlapping = takeWhile (\p -> isNothing $ lookupBridge p g) $ bridgePoints
    islandPoint    = nextPoint d . last $ notOverlapping


getRemoteIslandPoint :: Point -> BridgeDirection -> Game -> Point
getRemoteIslandPoint p d g = nextPoint d . last $ getBridgePoints p d g


couldBeOnBridge :: Point -> Point -> BridgeDirection -> Bool
couldBeOnBridge (Point x1 y1) (Point x2 y2) Up'    = x1 == x2 && y1 < y2
couldBeOnBridge (Point x1 y1) (Point x2 y2) Down'  = x1 == x2 && y1 > y2
couldBeOnBridge (Point x1 y1) (Point x2 y2) Left'  = x1 < x2 && y1 == y2
couldBeOnBridge (Point x1 y1) (Point x2 y2) Right' = x1 > x2 && y1 == y2


lookupBridge :: Point -> Game -> Maybe Bridge
lookupBridge searchP game = asum . map (searchGameBridges) $ getIslandPoints game
  where
    searchGameBridges :: Point -> Maybe Bridge
    searchGameBridges p = find (\b -> elem searchP $ getBridgePoints p (getBridgeDirection b) game)
                        . filter (couldBeOnBridge searchP p . getBridgeDirection) $ bridges
      where
        island  = getIsland p game
        bridges = getIslandBridges island


toString :: Game -> String
toString g = toStringLoop 0 0
  where
    toStringLoop :: Int -> Int -> String
    toStringLoop x y
        | y > (getYMax g) = ""
        | x > (getXMax g) = "\n" ++ toStringLoop 0 (y+1)
        | otherwise       = s ++ ' ' : toStringLoop (x+1) y
      where
        i = lookupIsland (Point x y) g
        b = lookupBridge (Point x y) g
        s = fromMaybe " " $ (islandToString <$> i) <|> (bridgeToString <$> b)


createGame :: [(Int, Int, Int)] -> Either String Game
createGame i = traverse createIsland i >>= createIslandMap >>= createGameFromMap
  where
    createIsland :: (Int, Int, Int) -> Either String (Point, Island)
    createIsland (x,y,v) = (\v -> ((Point x y), Island v [])) <$> verifyIslandValue v

    createIslandMap :: [(Point, Island)] -> Either String (Map.Map Point Island)
    createIslandMap i
        | mapSize == listSize = Right iMap
        | otherwise           = Left "Multiple islands exist at the same point"
      where
        iMap     = Map.fromList i
        mapSize  = Map.size iMap
        listSize = length i

    createGameFromMap :: (Map.Map Point Island) -> Either String Game
    createGameFromMap iMap
        | minX < 0 || minY < 0 = Left "Island points cannot be negative"
        | islandsTouching iMap = Left "Two islands cannot be directly next to each other"
        | otherwise            = Right (Game maxX maxY iMap)
      where
        maxX = maximum . map (getX) . Map.keys $ iMap
        maxY = maximum . map (getY) . Map.keys $ iMap
        minX = minimum . map (getX) . Map.keys $ iMap
        minY = minimum . map (getY) . Map.keys $ iMap

    islandsTouching :: (Map.Map Point Island) -> Bool
    islandsTouching iMap = any (hasAdjacentIsland) $ Map.keys iMap
      where
        hasAdjacentIsland :: Point -> Bool
        hasAdjacentIsland (Point x y) = any (`Map.member` iMap) [(Point x (y-1)),
                                                                 (Point x (y+1)),
                                                                 (Point (x-1) y),
                                                                 (Point (x+1) y)]


-- We do some trickery here. Instead of adding the bridge just to this
-- island, we also add the inverse of this bridge to the remote island.
-- For example, if we had to islands we wanted to connect with a bridge
-- like this:
-- 1       2
-- at the result of this call, we could have it look like:
-- 1->   <-2
-- This allows us to to very quickly calculate how many bridges are in an
-- island (without having to iterative over all the other islands) at the
-- expense of a little extra space and a slighly more complicated schema
addBridge :: Game -> Point -> Bridge -> Maybe Game
addBridge game point bridge = do
    remotePoint     <- lookupRemoteIslandPoint point (getBridgeDirection bridge) game
    let remoteBridge = reverseBridge bridge
    let island       = getIsland point game
    let remoteIsland = getIsland remotePoint game
    newIsland1      <- addBridgeToIsland bridge island
    newIsland2      <- addBridgeToIsland remoteBridge remoteIsland
    return $ updateIsland remotePoint newIsland2 $ updateIsland point newIsland1 game
  where
    addBridgeToIsland :: Bridge -> Island -> Maybe Island
    addBridgeToIsland b (Island v bs) = whenBool (not overFilled) newIsland
      where
        newIsland  = Island v (b : bs)
        overFilled = islandOverFilled newIsland

    updateIsland :: Point -> Island -> Game -> Game
    updateIsland p i (Game xMax yMax iMap) = Game xMax yMax (Map.insert p i iMap)


solve :: Game -> Maybe Game
solve game = solveLoop (getFirstPoint game) game
  where
    solveLoop :: Point -> Game -> Maybe Game
    solveLoop p g = case (getNextPoint g p) of
                        Nothing -> whenBool (isGameSolved g) g
                        Just p  -> asum . map (solveLoop p) $ getPossibleBridges g p


getNextPoint :: Game -> Point -> Maybe Point
getNextPoint g p = fst <$> p `Map.lookupGT` (getIslandPointMap g)


getFirstPoint :: Game -> Point
getFirstPoint g = fst . fromJust . Map.lookupGE (Point 0 0) $ getIslandPointMap g


getPossibleBridges :: Game -> Point -> [Game]
getPossibleBridges g p = filter (islandFilled . getIsland p) $ fillBridges p g
  where
    fillBridges :: Point -> Game -> [Game]
    fillBridges p g = nub $ g : (permutations >>= fillBridges p)
      where
        allPossibleBridges = liftA2 (Bridge) [Up', Down', Left', Right'] [Single, Double]
        permutations       = catMaybes . map (addBridge g p) $ allPossibleBridges


isGameSolved :: Game -> Bool
isGameSolved g = allIslandsFilled && allIslandsConnected
  where
    firstIsland         = getFirstPoint g
    allIslandsFilled    = all (islandFilled) . getIslands $ g
    allIslandsConnected = getIslandPointMap g == connectedIslands g


connectedIslands :: Game -> (Map.Map Point Island)
connectedIslands game = loop game Map.empty (getFirstPoint game)
  where
    loop :: Game -> (Map.Map Point Island) -> Point -> (Map.Map Point Island)
    loop g iMap p = foldr (Map.union) newIMap $ map (loop g newIMap) remotes
      where
        newIMap = Map.insert p (getIsland p g) iMap
        remotes = filter (`Map.notMember` newIMap) $ getRemotePoints p g


getRemotePoints :: Point -> Game -> [Point]
getRemotePoints p g = map (\d -> getRemoteIslandPoint p d g) directions
  where
    directions = map getBridgeDirection . getIslandBridges $ getIsland p g
