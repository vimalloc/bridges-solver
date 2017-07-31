import Data.List
import Data.Maybe
import Data.Foldable
import Control.Applicative
import Control.Monad
import qualified Data.Set as Set
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
    } deriving (Eq, Show)


data IslandValue = One
                 | Two
                 | Three
                 | Four
                 | Five
                 | Six
                 | Seven
                 | Eight deriving (Eq, Show, Ord)


data Point = Point
    { getX :: !Int
    , getY :: !Int
    } deriving (Eq, Show, Ord)


data Island = Island
    { getIslandValue   :: !IslandValue
    , getIslandBridges :: !(Set.Set Bridge)
    } deriving (Eq, Show, Ord)


data Game = Game
    { getXMax    :: !Int
    , getYMax    :: !Int
    , getIslandPointMap :: !(Map.Map Point Island)
    } deriving (Eq, Show)


-- Define our own ordering test here. We are using a set of bridges for the
-- island, and we don't care if the bridge is a single or double there, only
-- what direction it is leaving the island from. This will insure we cannot
-- have duplicate bridges on an island without any addition checks on our part
instance Ord Bridge where
    (Bridge d1 _) `compare` (Bridge d2 _) = d1 `compare` d2


intToIslandValue :: Int -> Either String IslandValue
intToIslandValue 1 = Right One
intToIslandValue 2 = Right Two
intToIslandValue 3 = Right Three
intToIslandValue 4 = Right Four
intToIslandValue 5 = Right Five
intToIslandValue 6 = Right Six
intToIslandValue 7 = Right Seven
intToIslandValue 8 = Right Eight
intToIslandValue _ = Left "Island values must be between 1 and 8 inclusive"


islandValueInt :: Island -> Int
islandValueInt (Island One _)   = 1
islandValueInt (Island Two _)   = 2
islandValueInt (Island Three _) = 3
islandValueInt (Island Four _)  = 4
islandValueInt (Island Five _)  = 5
islandValueInt (Island Six  _)  = 6
islandValueInt (Island Seven _) = 7
islandValueInt (Island Eight _) = 8


bridgeToInt :: Bridge -> Int
bridgeToInt (Bridge _ Single) = 1
bridgeToInt (Bridge _ Double) = 2


numBridges :: Island -> Int
numBridges = Set.foldr ((+) . bridgeToInt) 0 . getIslandBridges


islandOverFilled :: Island -> Bool
islandOverFilled i = numBridges i > islandValueInt i


islandFilled :: Island -> Bool
islandFilled i = numBridges i == islandValueInt i


isIsland :: Point -> Game -> Bool
isIsland p g = p `Map.member` (getIslandPointMap g)


-- Terminal codes to make result blue and bold
islandToString :: Island -> String
islandToString (Island One _)   = "\ESC[94m\ESC[1m1\ESC[0m"
islandToString (Island Two _)   = "\ESC[94m\ESC[1m2\ESC[0m"
islandToString (Island Three _) = "\ESC[94m\ESC[1m3\ESC[0m"
islandToString (Island Four _)  = "\ESC[94m\ESC[1m4\ESC[0m"
islandToString (Island Five _)  = "\ESC[94m\ESC[1m5\ESC[0m"
islandToString (Island Six _)   = "\ESC[94m\ESC[1m6\ESC[0m"
islandToString (Island Seven _) = "\ESC[94m\ESC[1m7\ESC[0m"
islandToString (Island Eight _) = "\ESC[94m\ESC[1m8\ESC[0m"


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


traverseBridge :: BridgeDirection -> Point -> Point
traverseBridge Up' (Point x y)    = (Point x (y-1))
traverseBridge Down' (Point x y)  = (Point x (y+1))
traverseBridge Left' (Point x y)  = (Point (x-1) y)
traverseBridge Right' (Point x y) = (Point (x+1) y)


pointCouldBeOnBridge :: Point -> Point -> BridgeDirection -> Bool
pointCouldBeOnBridge (Point x1 y1) (Point x2 y2) Up'    = x1 == x2 && y1 < y2
pointCouldBeOnBridge (Point x1 y1) (Point x2 y2) Down'  = x1 == x2 && y1 > y2
pointCouldBeOnBridge (Point x1 y1) (Point x2 y2) Left'  = x1 < x2 && y1 == y2
pointCouldBeOnBridge (Point x1 y1) (Point x2 y2) Right' = x1 > x2 && y1 == y2


findRemoteIslandPoint :: Point -> BridgeDirection -> Game -> Maybe Point
findRemoteIslandPoint p d g = find (`isIsland` g) . takeWhile (onBridge) $ allPoints
  where
    allPoints  = iterate (traverseBridge d) $ traverseBridge d p
    isBridge p = isNothing $ lookupBridge p g
    onBoard p  = (getX p <= getXMax g) && (getX p >= 0) &&
                 (getY p <= getYMax g) && (getY p >= 0)
    onBridge p = onBoard p && isBridge p


-- Cannot use findRemoteIslandPoint here, as that will return nothing if it
-- crosses a bridge, and this assumes a bridge has already been placed and
-- are following it to the remote island
getRemoteIslandPoint :: Point -> Game -> BridgeDirection -> Point
getRemoteIslandPoint p g d = fromJust $ find (`isIsland` g) allPoints
  where
    allPoints = iterate (traverseBridge d) $ traverseBridge d p


getBridgePoints :: Point -> BridgeDirection -> Game -> [Point]
getBridgePoints p d g =  foldr (:) [] . takeWhile (notIsland g) $ allBridgePoints
  where
    allBridgePoints  = iterate (traverseBridge d) $ traverseBridge d p
    notIsland g p   = not (isIsland p g)


-- TODO think i can do better here. I don't need to check for the actual points,
--      I just need to filter out so only the bridges on the same horizontal or
--      vertical line are left in a list, then select the one that is closes to
--      the point. Think that should be faster as im no longer iterating
--      bridges (twice for that matter), but just doing math on points, and
--      filtering is cheap.
lookupBridge :: Point -> Game -> Maybe Bridge
lookupBridge p g = asum . map (test) $ getIslandPoints g
  where
    test :: Point -> Maybe Bridge  -- TODO better name for this
    test islandPoint = find (pointOnBridge islandPoint) bridges
      where
        bridges = getIslandBridges $ getIsland islandPoint g

    -- TODO the p / p' naming here is confusing. Do better.
    pointOnBridge :: Point -> Bridge -> Bool
    pointOnBridge p' b = couldBeOnBridge && p `elem` bridgePoints
      where
        direction       = getBridgeDirection b
        couldBeOnBridge = pointCouldBeOnBridge p p' direction
        bridgePoints    = getBridgePoints p' (getBridgeDirection b) g


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
    createIsland (x,y,v) = (\v -> ((Point x y), Island v Set.empty)) <$> intToIslandValue v

    createIslandMap :: [(Point, Island)] -> Either String (Map.Map Point Island)
    createIslandMap i
        | mapSize == listSize = Right iMap
        | otherwise           = Left "Multiple islands exist at the same point"
      where
        iMap = Map.fromList i
        mapSize = Map.size iMap
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
--
-- TODO Can I make this more elegant without a do block here?
addBridge :: Game -> Point -> Bridge -> Maybe Game
addBridge game point bridge = do
    remotePoint     <- findRemoteIslandPoint point (getBridgeDirection bridge) game
    let remoteBridge = reverseBridge bridge
    let island       = getIsland point game
    let remoteIsland = getIsland remotePoint game
    newIsland1      <- addBridgeToIsland bridge island
    newIsland2      <- addBridgeToIsland remoteBridge remoteIsland
    return $ updateIsland remotePoint newIsland2 $ updateIsland point newIsland1 game
  where
    addBridgeToIsland :: Bridge -> Island -> Maybe Island
    addBridgeToIsland bridge (Island v b)
        | bridge `Set.member` b      = Nothing
        | islandOverFilled newIsland = Nothing
        | otherwise                  = Just newIsland
      where
        newIsland = Island v (bridge `Set.insert` b)

    updateIsland :: Point -> Island -> Game -> Game
    updateIsland p i (Game xMax yMax iMap) = Game xMax yMax (Map.insert p i iMap)


solve :: Game -> Maybe Game
solve game = solveLoop (getFirstPoint game) game
  where
    solveLoop :: Point -> Game -> Maybe Game
    solveLoop p g = case (getNextPoint g p) of
                        Nothing -> fromBool (isGameSolved g) g
                        Just p  -> asum . map (solveLoop p) $ getPossibleBridges g p


getNextPoint :: Game -> Point -> Maybe Point
getNextPoint g p = fst <$> p `Map.lookupGT` (getIslandPointMap g)


getFirstPoint :: Game -> Point
getFirstPoint g = fst . fromJust . Map.lookupGE (Point 0 0) $ getIslandPointMap g


getPossibleBridges :: Game -> Point -> [Game]
getPossibleBridges g p = filter (islandFilled . getIsland p) $ fillBridges p g
  where
    fillBridges :: Point -> Game -> [Game]
    fillBridges p g = nub $ g : (perms >>= fillBridges p)
      where
        bridgeCombinations = liftA2 (Bridge) [Up', Down', Left', Right'] [Single, Double]
        perms = catMaybes . map (addBridge g p) $ bridgeCombinations


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
getRemotePoints p g = map (getRemoteIslandPoint p g . getBridgeDirection) bridges
  where
    bridges = Set.toList . getIslandBridges $ getIsland p g


fromBool :: Bool -> a -> Maybe a
fromBool False _ = Nothing
fromBool True a  = Just a


-- Helper function so I can more easily play with createBridges in repl
fromRight :: Either a b -> b
fromRight (Right b) = b

testGame1 = fromRight $ createGame [(0, 0, 1), (2, 0, 1), (0, 2, 1), (2, 2, 1)]

testGame2 = fromRight $ createGame [(1, 0, 2), (5, 0, 4), (9, 0, 4), (0, 1, 1),
                                    (1, 2, 4), (4, 2, 3), (7, 2, 4), (9, 2, 5),
                                    (7, 4, 2), (5, 5, 4), (9, 5, 4), (1, 7, 1),
                                    (4, 7, 1), (7, 7, 1), (0, 8, 3), (5, 8, 4),
                                    (1, 9, 1), (3, 9, 2), (7, 9, 4), (9, 9, 4)]


-- TODO this is just a dummy main for testing stuff. What I want to do next
--      is make this stuff a module (export createGame, solvePuzzle, pprint)
--      and build another application on top of this which goes out to the
--      puzzle bridges website and downloads/solves games from there. All IO
--      will happen there.
main :: IO ()
main = do
    let game = testGame2
    let width = (getXMax game) * 2 + 1
    let border = replicate width '~'  ++ "\n"
    putStrLn border
    putStrLn . toString $ game
    putStrLn border
    putStrLn . toString . fromJust . solve $ game
    putStrLn border
