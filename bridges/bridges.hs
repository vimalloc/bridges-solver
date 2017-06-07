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
    { getIslands :: ![Island]
    , getXMax    :: !Int
    , getYMax    :: !Int
    , getIslandPointMap :: !(Map.Map Point Island)
    } deriving (Eq, Show)

-- Define our own ordering test here. We are using a set of bridges for the
-- island, and we don't care if the bridge is a single or double there, only
-- what direction it is leaving the island from. This will insure we cannot
-- have duplicate bridges on an island without any addition checks on our part
instance Ord Bridge where
    (Bridge d1 _) `compare` (Bridge d2 _) = d1 `compare` d2

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

getIslandX :: Island -> Int
getIslandX = getX . getIslandPoint

getIslandY :: Island -> Int
getIslandY = getY . getIslandPoint

islandsMaxX :: [Island] -> Int
islandsMaxX = maximum . map getX . map getIslandPoint

islandsMaxY :: [Island] -> Int
islandsMaxY = maximum . map getY . map getIslandPoint

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
getRemoteIsland i direction game = getRemoteIslandLoop startPoint incPoint
  where
    incPoint     = traverseBridge direction
    startPoint   = incPoint $ getIslandPoint i

    getRemoteIslandLoop :: Point -> (Point -> Point) -> Maybe Island
    getRemoteIslandLoop p incPoint
        | (getX p > getXMax game) = Nothing
        | (getY p > getYMax game) = Nothing
        | (getX p < 0)            = Nothing
        | (getY p < 0)            = Nothing
        | otherwise               = case possibleIsland of
                                        Just i  -> Just i
                                        Nothing -> getRemoteIslandLoop nextPoint incPoint
      where
        possibleIsland = p `Map.lookup` (getIslandPointMap game)
        nextPoint      = incPoint p

getBridgePoints :: Island -> Bridge -> Game -> [Point]
getBridgePoints island bridge game
    | not bridgeInIsland = error "Bridge not on island"
    | otherwise          = getBridgePointsLoop startPoint incPoint
  where
    bridgeInIsland = bridge `Set.member` (getIslandBridges island)
    incPoint       = traverseBridge $ getBridgeDirection bridge
    startPoint     = incPoint $ getIslandPoint island

    getBridgePointsLoop :: Point -> (Point -> Point) -> [Point]
    getBridgePointsLoop p incPoint
        | (getX p > getXMax game) = error "Bridge does not connecto to an island"
        | (getY p > getYMax game) = error "Bridge does not connecto to an island"
        | (getX p < 0)            = error "Bridge does not connecto to an island"
        | (getY p < 0)            = error "Bridge does not connecto to an island"
        | isIsland                = []
        | otherwise               = p : getBridgePointsLoop nextP incPoint
      where
        isIsland = p `Map.member` (getIslandPointMap game)
        nextP = incPoint p

islandAtPoint :: Point -> Game -> Maybe Island
islandAtPoint point game = point `Map.lookup` (getIslandPointMap game)

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
createIslands i = do
    islands <- createIslandsGo i  -- Can I make this a map instead?
    let xMax = islandsMaxX islands
    let yMax = islandsMaxY islands
    let iMap = Map.fromList $ [(getIslandPoint i, i) | i <- islands]
    let game = Game islands xMax yMax iMap
    when (hasDuplicateIslands game) $ Left "Multiple islands exist at the same point"
    when (hasNoSpaceForBridges game) $ Left "Two islands are directly next to each other"
    return game
  where
    createIslandsGo :: [(Int, Int, Int)] -> Either String [Island]
    createIslandsGo []     = Right []
    createIslandsGo (x:xs) = do
        island     <- createIsland x
        nextIsland <- createIslandsGo xs
        return $ island : nextIsland

    createIsland :: (Int, Int, Int) -> Either String Island
    createIsland (x,y,v) = do
        value <- intToIslandValue v
        return $ Island (Point x y) value Set.empty

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

    hasDuplicateIslands :: Game -> Bool
    hasDuplicateIslands game = listSize /= setSize
      where
        listSize = length $ getIslands game
        setSize  = Map.size $ getIslandPointMap game

    hasNoSpaceForBridges :: Game -> Bool
    hasNoSpaceForBridges game = any hasNoSpaceForBridge $ getIslands game
      where
        hasNoSpaceForBridge :: Island -> Bool
        hasNoSpaceForBridge island
            | up `Map.member`    points = True
            | down `Map.member`  points = True
            | left `Map.member`  points = True
            | right `Map.member` points = True
            | otherwise                 = False
          where
            x      = getX . getIslandPoint $ island
            y      = getY . getIslandPoint $ island
            up     = Point x (y-1)
            down   = Point x (y+1)
            left   = Point (x-1) y
            right  = Point (x+1) y
            points = getIslandPointMap game


-- TODO does it actually make sense to return the same, unmodified islands here?
--checkSpaceForBridges :: Island -> Either String Island
--


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
