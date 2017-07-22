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
numBridges i = Set.foldr (+) 0 $ Set.map (bridgeToInt) (getIslandBridges i)


islandOverFilled :: Island -> Bool
islandOverFilled island = numBridges island > islandValueInt island


islandToChar :: Island -> Char
islandToChar (Island _ One _)   = '1'
islandToChar (Island _ Two _)   = '2'
islandToChar (Island _ Three _) = '3'
islandToChar (Island _ Four _)  = '4'
islandToChar (Island _ Five _)  = '5'
islandToChar (Island _ Six _)   = '6'
islandToChar (Island _ Seven _) = '7'
islandToChar (Island _ Eight _) = '8'


bridgeToChar :: Bridge -> Char
bridgeToChar (Bridge Up' Single)    = '|'
bridgeToChar (Bridge Down' Single)  = '|'
bridgeToChar (Bridge Up' Double)    = '‖'
bridgeToChar (Bridge Down' Double)  = '‖'
bridgeToChar (Bridge Left' Single)  = '―'
bridgeToChar (Bridge Right' Single) = '―'
bridgeToChar (Bridge Left' Double)  = '═'
bridgeToChar (Bridge Right' Double) = '═'


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
        | otherwise           = c : " " ++ pprintLoop (x+1) y game
      where
        i = islandAtPoint (Point x y) game
        b = bridgeAtPoint (Point x y) game
        c = fromMaybe ' ' $ (islandToChar <$> i) <|> (bridgeToChar <$> b)


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
    updateIslands game i1 i2 = updateIsland i2 $ updateIsland i1 game

    updateIsland :: Island -> Game -> Game
    updateIsland island (Game xMax yMax islandMap) = Game xMax yMax updatedIslands
      where
        updatedIslands = Map.insert (getIslandPoint island) island islandMap


-- Solving stragety
-- * Start at the first island. Need to try each combination of bridges that
-- * Need to try every possible bridge combinations to fill up the island
-- * If the island cannot be filled with bridges, recurse backwors, or
--   in this case return Nothing, as the game cannot be solved
-- * If the island can be filled with bridges, fill the island then
--   recurse forward to the next island
-- * If the return value of this function is Nothing, that means that
--   every the puzzle cannot be solved, so try to fill the island with a
--   different configuration of bridges and go forward again
-- * If no more valid combinations of bridges can be found for this island
--   return Nothing and let the previous island re-arrange itself.
-- * If this island can be filled with bridges, and there is no other
--   islands in the game grid, run a function that checks to make sure
--   all the bridges are connectied, which should return 'Just game' or
--   nothing depending on if it is valid. If it is valid, then we just
--   solved the puzzle. Congrats.
solve :: Game -> Maybe Game
solve game = solveLoop game (getFirstIsland game)
  where
    solveLoop :: Game -> Island -> Maybe Game
    solveLoop game island = Just game
      where
        nextIsland = getNextIsland game island


getFirstIsland :: Game -> Island
getFirstIsland game = snd . fromJust $ smallestPoint `Map.lookupGE` islandMap
  where
    smallestPoint = Point 0 0
    islandMap     = getIslandPointMap game


getNextIsland :: Game -> Island -> Maybe Island
getNextIsland game island = snd <$> islandPoint `Map.lookupGT` islandMap
  where
    islandPoint = getIslandPoint island
    islandMap   = getIslandPointMap game


-- TODO can I make this better with list monads? Looks like bine for list is
--      basically concat map which I'm already doing, so probably
-- TODO Better way to come up with all bridge combinations
-- TODO final step, filter out only islands that are fully filled
-- TODO is it possible that the same combination of bridges could be generated
--      multiple time. Yes, yes it is. We should remove those duplicates so we
--      don't waste cpu time running calculations we have already done (could
--      we just use a set instead of a list for this or something? Or memoization?)
fillBridges :: Island -> Game -> [Game]
fillBridges i g
    | null perms = [g]
    | otherwise  = (g : perms) ++ concat (map (fillBridges i) perms)
  where
    islandP  = getIslandPoint i
    allPerms = [addBridge g i (Bridge Up' Single),
                addBridge g i (Bridge Up' Double),
                addBridge g i (Bridge Down' Single),
                addBridge g i (Bridge Down' Double),
                addBridge g i (Bridge Left' Single),
                addBridge g i (Bridge Left' Double),
                addBridge g i (Bridge Right' Single),
                addBridge g i (Bridge Right' Double)]
    perms = map (fromJust) $ filter (isJust) allPerms


-- Attempt to solve the puzzle
--  * Making sure no bridges overlap
--  * Making sure all islands have the correct number of bridges coming from them
--  * Making sure all islands are connected
--
-- Have this module only expor
--  * createGame :: [Point] -> Game
--  * solvePuzzle :: Game -> Maybe Game
--  * ppring :: Game -> String
--
-- If I understnad the import/export system correctly, this will not allow
-- users to modify the internals of the [Island] and create an invalid or
-- impossible situation. I'm not sure if this is like java where they *really*
-- can't modify the internals, or if it's like python where they are just
-- encouraged not to. But either way, that should be good enough for this.
--
-- Finally, build another program on top of this which include the main
-- function, imports this module, and fetches games from the puzzle bridges
-- website, and solves them. All the IO stuff happens here

-- Helper function so I can more easily play with createBridges in repl
fromRight :: Either a b -> b
fromRight (Right b) = b

testGame = fromRight $ createIslands [(0,0,1), (2,0,1), (4,0,3), (0, 2, 3), (4, 2, 5), (2, 4, 1), (4, 4, 2)]

