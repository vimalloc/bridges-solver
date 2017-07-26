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
    { getIslandPoint   :: !Point
    , getIslandValue   :: !IslandValue
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


islandValueToInt :: IslandValue -> Int
islandValueToInt One   = 1
islandValueToInt Two   = 2
islandValueToInt Three = 3
islandValueToInt Four  = 4
islandValueToInt Five  = 5
islandValueToInt Six   = 6
islandValueToInt Seven = 7
islandValueToInt Eight = 8


bridgeToInt :: Bridge -> Int
bridgeToInt (Bridge _ Single) = 1
bridgeToInt (Bridge _ Double) = 2


islandValueInt :: Island -> Int
islandValueInt = islandValueToInt . getIslandValue


numBridges :: Island -> Int
numBridges i = foldr (+) 0 $ map (bridgeToInt) (Set.toList $ getIslandBridges i)


islandOverFilled :: Island -> Bool
islandOverFilled island = numBridges island > islandValueInt island


islandFilled :: Island -> Bool
islandFilled island = numBridges island == islandValueInt island


-- Terminal codes to make result blue and bold
islandToString :: Island -> String
islandToString (Island _ One _)   = "\ESC[94m\ESC[1m1\ESC[0m"
islandToString (Island _ Two _)   = "\ESC[94m\ESC[1m2\ESC[0m"
islandToString (Island _ Three _) = "\ESC[94m\ESC[1m3\ESC[0m"
islandToString (Island _ Four _)  = "\ESC[94m\ESC[1m4\ESC[0m"
islandToString (Island _ Five _)  = "\ESC[94m\ESC[1m5\ESC[0m"
islandToString (Island _ Six _)   = "\ESC[94m\ESC[1m6\ESC[0m"
islandToString (Island _ Seven _) = "\ESC[94m\ESC[1m7\ESC[0m"
islandToString (Island _ Eight _) = "\ESC[94m\ESC[1m8\ESC[0m"


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
reverseBridge (Bridge Up' val)    = Bridge Down' val
reverseBridge (Bridge Down' val)  = Bridge Up' val
reverseBridge (Bridge Left' val)  = Bridge Right' val
reverseBridge (Bridge Right' val) = Bridge Left' val


getIslands :: Game -> [Island]
getIslands game = Map.elems $ getIslandPointMap game


islandsMaxX :: [Island] -> Int
islandsMaxX = maximum . map getX . map getIslandPoint


islandsMaxY :: [Island] -> Int
islandsMaxY = maximum . map getY . map getIslandPoint


getIsland :: Point -> Game -> Maybe Island
getIsland point game = point `Map.lookup` (getIslandPointMap game)


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


getRemoteIsland :: Island -> BridgeDirection -> Game -> Maybe Island
getRemoteIsland i direction game = getRemoteIslandLoop startPoint
  where
    incPoint   = traverseBridge direction
    startPoint = incPoint $ getIslandPoint i

    getRemoteIslandLoop :: Point -> Maybe Island
    getRemoteIslandLoop p
        | (getX p > getXMax game) = Nothing
        | (getY p > getYMax game) = Nothing
        | (getX p < 0)            = Nothing
        | (getY p < 0)            = Nothing
        | isBridge                = Nothing
        | otherwise               = case maybeIsland of
                                        Just i  -> Just i
                                        Nothing -> getRemoteIslandLoop nextPoint
      where
        maybeIsland = p `getIsland` game
        isBridge    = isJust $ bridgeAtPoint p game
        nextPoint   = incPoint p


getBridgePoints :: Island -> Bridge -> Game -> [Point]
getBridgePoints island bridge game
    | not bridgeInIsland = error "Bridge not on island"
    | otherwise          = getBridgePointsLoop startPoint
  where
    bridgeInIsland = bridge `Set.member` (getIslandBridges island)
    incPoint       = traverseBridge $ getBridgeDirection bridge
    startPoint     = incPoint $ getIslandPoint island

    getBridgePointsLoop :: Point -> [Point]
    getBridgePointsLoop p
        | (getX p > getXMax game) = error "Bridge does not connecto to an island"
        | (getY p > getYMax game) = error "Bridge does not connecto to an island"
        | (getX p < 0)            = error "Bridge does not connecto to an island"
        | (getY p < 0)            = error "Bridge does not connecto to an island"
        | isIsland                = []
        | otherwise               = p : getBridgePointsLoop nextP
      where
        isIsland = p `Map.member` (getIslandPointMap game)
        nextP = incPoint p


islandAtPoint :: Point -> Game -> Maybe Island
islandAtPoint point game = point `Map.lookup` (getIslandPointMap game)


-- TODO think i can do better here. I don't need to check for the actual points,
--      I just need to filter out so only the bridges on the same horizontal or
--      vertical line are left in a list, then select the one that is closes to
--      the point. Think that should be significatnly faster as im no longer
--      iterating bridges, but just doing math on points (and filter is o(n))
bridgeAtPoint :: Point -> Game -> Maybe Bridge
bridgeAtPoint point game = asum . map getBridgeAtPointFromIsland $ getIslands game
  where
    getBridgeAtPointFromIsland :: Island -> Maybe Bridge
    getBridgeAtPointFromIsland i = find (pointOnBridge i) $ getIslandBridges i

    pointOnBridge :: Island -> Bridge -> Bool
    pointOnBridge island bridge = couldBeOnBridge && point `elem` bridgePoints
      where
        islandPoint     = getIslandPoint island
        direction       = getBridgeDirection bridge
        couldBeOnBridge = pointCouldBeOnBridge point islandPoint direction
        bridgePoints    = getBridgePoints island bridge game


pprint :: Game -> String
pprint game = pprintLoop 0 0 game
  where
    pprintLoop :: Int -> Int -> Game -> String
    pprintLoop x y game
        | y > (getYMax game)  = ""
        | x > (getXMax game)  = "\n" ++ pprintLoop 0 (y+1) game
        | otherwise           = c ++ " " ++ pprintLoop (x+1) y game
      where
        i = islandAtPoint (Point x y) game
        b = bridgeAtPoint (Point x y) game
        c = fromMaybe " " $ (islandToString <$> i) <|> (bridgeToString <$> b)


-- TODO don't allow islands at negative points
createIslands :: [(Int, Int, Int)] -> Either String Game
createIslands i = traverse createIsland i >>= createGame
  where
    createIsland :: (Int, Int, Int) -> Either String Island
    createIsland (x,y,v) = (\v -> Island (Point x y) v Set.empty) <$> intToIslandValue v

    createGame :: [Island] -> Either String Game
    createGame islands
        | hasDuplicateIslands islands game  = Left "Multiple islands exist at the same point"
        | hasNoSpaceForBridges islands game = Left "Two islands exists without room for a bridge between them"
        | otherwise                         = Right game
      where
        iMap = Map.fromList $ [(getIslandPoint i, i) | i <- islands]
        game = Game (islandsMaxX islands) (islandsMaxY islands) iMap

    hasDuplicateIslands :: [Island] -> Game -> Bool
    hasDuplicateIslands islands game = listSize /= setSize
      where
        listSize = length islands
        setSize  = Map.size $ getIslandPointMap game

    hasNoSpaceForBridges :: [Island] -> Game -> Bool
    hasNoSpaceForBridges islands game = any (hasNoSpaceForBridge) islands
      where
        hasNoSpaceForBridge :: Island -> Bool
        hasNoSpaceForBridge (Island (Point x y) _ _)
            | (Point x (y-1)) `Map.member` points = True
            | (Point x (y+1)) `Map.member` points = True
            | (Point (x-1) y) `Map.member` points = True
            | (Point (x+1) y) `Map.member` points = True
            | otherwise                           = False
          where
            points = getIslandPointMap game


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
addBridge :: Game -> Island -> Bridge -> Maybe Game
addBridge game island bridge = updateIslands game <$> newIsland1 <*> newIsland2
  where
    remote_island = getRemoteIsland island (getBridgeDirection bridge) game
    newIsland1    = addBridgeToIsland bridge island
    newIsland2    = remote_island >>= addBridgeToIsland (reverseBridge bridge)

    addBridgeToIsland :: Bridge -> Island -> Maybe Island
    addBridgeToIsland bridge (Island p v b)
        | bridge `Set.member` b      = Nothing
        | islandOverFilled newIsland = Nothing
        | otherwise                  = Just newIsland
      where
        newIsland = Island p v (bridge `Set.insert` b)

    updateIslands :: Game -> Island -> Island -> Game
    updateIslands game i1 i2 = updateIsland i2 . updateIsland i1 $ game

    updateIsland :: Island -> Game -> Game
    updateIsland island (Game xMax yMax islandMap) = Game xMax yMax updatedIslands
      where
        updatedIslands = Map.insert (getIslandPoint island) island islandMap


-- TODO move this into single recursive loop (was just easier to reason about
--      this way when developint it)
solve :: Game -> Maybe Game
solve game = solveLoop1 (getFirstIsland game) game
  where
    {-
    solveLoop :: [Game] -> Point -> Maybe Game
    solveLoop [] _   = Nothing
    solveLoop (g:gs) p
        | isGameSolved g       = Just g
        | isNothing nextIsland = Nothing
        | otherwise            = case (getPossibleBridges p g) of
                                   []  -> solveLoop gs p
                                   gs' -> solveLoop gs' $ fromJust nextIsland
      where
        nextIsland = getNextIsland g p
    -}

    solveLoop1 :: Point -> Game -> Maybe Game
    solveLoop1 p g = case (getNextIsland g p) of
                         Nothing -> fromBool (isGameSolved g) g
                         Just i  -> solveLoop2 (getPossibleBridges p g) i

    solveLoop2 :: [Game] -> Point -> Maybe Game
    solveLoop2 gs p = fromJust <$> (find (isJust) . map (solveLoop1 p) $ gs)


getNextIsland :: Game -> Point -> Maybe Point
getNextIsland g p = fst <$> p `Map.lookupGT` (getIslandPointMap g)


getFirstIsland :: Game -> Point
getFirstIsland g = fst . fromJust . Map.lookupGE (Point 0 0) $ getIslandPointMap g


getPossibleBridges :: Point -> Game -> [Game]
getPossibleBridges p g = filter (islandFilled . fromJust . getIsland p) $ fillBridges p g
  where
    fillBridges :: Point -> Game -> [Game]
    fillBridges p g = nub $ g : (perms >>= fillBridges p)
      where
        island = fromJust $ getIsland p g
        perms = catMaybes . map (addBridge g island) $ [Bridge Up' Single,
                                                        Bridge Up' Double,
                                                        Bridge Down' Single,
                                                        Bridge Down' Double,
                                                        Bridge Left' Single,
                                                        Bridge Left' Double,
                                                        Bridge Right' Single,
                                                        Bridge Right' Double]


-- TODO account for loops in the game
isGameSolved :: Game -> Bool
isGameSolved g = all (islandFilled) $ getIslands g


fromBool :: Bool -> a -> Maybe a
fromBool False _ = Nothing
fromBool True a  = Just a


-- Have this module only expor
--  * createGame :: [Point] -> Game
--  * solvePuzzle :: Game -> Maybe Game
--  * ppring :: Game -> String
--
-- Finally, build another program on top of this which include the main
-- function, imports this module, and fetches games from the puzzle bridges
-- website, and solves them. All the IO stuff happens here


-- Helper function so I can more easily play with createBridges in repl
fromRight :: Either a b -> b
fromRight (Right b) = b

testGame = fromRight $ createIslands [(0,0,1), (2,0,1), (4,0,3), (0, 2, 3), (4, 2, 5), (2, 4, 1), (4, 4, 2)]
testGame2 = fromRight $ createIslands [(1, 0, 2),
                                       (5, 0, 4),
                                       (9, 0, 4),
                                       (0, 1, 1),
                                       (1, 2, 4),
                                       (4, 2, 3),
                                       (7, 2, 4),
                                       (9, 2, 5),
                                       (7, 4, 2),
                                       (5, 5, 4),
                                       (9, 5, 4),
                                       (1, 7, 1),
                                       (4, 7, 1),
                                       (7, 7, 1),
                                       (0, 8, 3),
                                       (5, 8, 4),
                                       (1, 9, 1),
                                       (3, 9, 2),
                                       (7, 9, 4),
                                       (9, 9, 4)]
