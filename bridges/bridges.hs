import Data.List
import Data.Maybe
import Data.Foldable
import Control.Applicative
import qualified Data.Set as Set


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

-- Define our own ordering test here. We are using a set of bridges for the
-- island, and we don't care if the bridge is a single or double there, only
-- what direction it is leaving the island from. This will insure we cannot
-- have duplicate bridges on an island without any addition checks on our part
instance Ord Bridge where
    (Bridge d1 _) `compare` (Bridge d2 _) = d1 `compare` d2

getIslandX :: Island -> Int
getIslandX = getX . getIslandPoint

getIslandY :: Island -> Int
getIslandY = getY . getIslandPoint

islandsMaxX :: [Island] -> Int
islandsMaxX = maximum . map getX . map getIslandPoint

islandsMaxY :: [Island] -> Int
islandsMaxY = maximum . map getY . map getIslandPoint

traverseBridge :: Bridge -> Point -> Point
traverseBridge (Bridge Up' _) (Point x y)    = (Point x (y-1))
traverseBridge (Bridge Down' _) (Point x y)  = (Point x (y+1))
traverseBridge (Bridge Left' _) (Point x y)  = (Point (x-1) y)
traverseBridge (Bridge Right' _) (Point x y) = (Point (x+1) y)

pointCouldExistOnBridge :: Point -> Island -> Bridge -> Bool
pointCouldExistOnBridge p i b = let x  = getX p
                                    y  = getY p
                                    x' = getIslandX i
                                    y' = getIslandY i
                                in case getBridgeDirection b of
                                       Up'    -> x == x' && y < y'
                                       Down'  -> x == x' && y > y'
                                       Left'  -> x < x' && y == y'
                                       Right' -> x > x' && y == y'

getBridgePoints :: Island -> Bridge -> [Island] -> [Point]
getBridgePoints island bridge allIslands
    | not bridgeInIsland = error "Bridge not on island"
    | otherwise          = getBridgePointsLoop startPoint incPoint
  where
    bridgeInIsland = bridge `Set.member` (getIslandBridges island)
    xMax           = islandsMaxX allIslands
    yMax           = islandsMaxY allIslands
    islandPoints   = Set.fromList . map getIslandPoint $ allIslands
    incPoint       = traverseBridge bridge
    startPoint     = incPoint $ getIslandPoint island

    getBridgePointsLoop :: Point -> (Point -> Point) -> [Point]
    getBridgePointsLoop p incPoint
        | (getX p) > xMax             = []  -- TODO should this be an error?
        | (getY p) > yMax             = []  -- TODO should this be an error?
        | p `Set.member` islandPoints = []
        | otherwise                   = p : getBridgePointsLoop nextP incPoint
      where
        nextP = incPoint p

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

pprint :: [Island] -> String
pprint islands = pprintLoop 0 0 islands
  where
    xMax = islandsMaxX islands
    yMax = islandsMaxY islands

    pprintLoop :: Int -> Int -> [Island] -> String
    pprintLoop x y islands
        | y > yMax  = ""
        | x > xMax  = "\n" ++ pprintLoop 0 (y+1) islands
        | otherwise = c : " " ++ pprintLoop (x+1) y islands
      where
        i = islandAtPoint (Point x y) islands
        b = bridgeAtPoint (Point x y) islands
        c = fromMaybe ' ' $ (islandToChar <$> i) <|> (bridgeToChar <$> b)

islandAtPoint :: Point -> [Island] -> Maybe Island
islandAtPoint p = find (\ i -> getIslandPoint i == p)

bridgeAtPoint :: Point -> [Island] -> Maybe Bridge
bridgeAtPoint point allIslands = asum . map getBridgeAtPointFromIsland $ allIslands
  where
    getBridgeAtPointFromIsland :: Island -> Maybe Bridge
    getBridgeAtPointFromIsland i = find (pointOnBridge i) bridges
      where
        bridges = getIslandBridges i

    pointOnBridge :: Island -> Bridge -> Bool
    pointOnBridge island bridge = couldBeOnBridge && point `elem` bridgePoints
      where
        couldBeOnBridge = pointCouldExistOnBridge point island bridge
        bridgePoints = getBridgePoints island bridge allIslands

-- TODO make this tail recursive
-- TODO could we do this same thing with Applicative instead of monads?
-- TODO verify there are no duplicate island points
createIslands :: [(Int, Int, Int)] -> Either String [Island]
createIslands []           = Right []
createIslands ((x,y,v):xs) = do
    value       <- intToIslandValue v
    let point   = Point x y
    let bridges = Set.fromList []
    let island  = Island point value bridges
    nextIsland  <- createIslands xs
    return $ island : nextIsland

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

-- Import bridge from file/stdin (what format do I want for this?)
--   * Just the points of islands
--
-- Verify bridge is valid before allowing you to attempt to solve it:
--  * At least two islands
--  * Making sure all bridges connect to another island (don't go to infinity)
--  * Make sure no island has more bridges coming to it
--  * Removing duplicate bridges? (depends on how we allow importing of bridges)
--  * Make sure no bridges are intersecting
--
-- Attempt to solve the puzzle
--  * Making sure no bridges overlap
--  * Making sure all islands have the correct number of bridges coming from them
--  * Making sure all islands are connected
--
-- Have this module only expor
--  * createIslands :: [Point] -> [Island]
--  * solvePuzzle :: [Island] -> Maybe [Island]
--  * ppring :: [Island] -> String
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
