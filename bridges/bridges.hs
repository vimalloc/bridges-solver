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

data Bridge = Bridge BridgeDirection BridgeValue deriving (Eq, Show)

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
    { getIslandPoint :: !Point
    , getIslandValue :: !IslandValue
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
                                in case b of
                                       (Bridge Up' _)    -> x == x' && y < y'
                                       (Bridge Down' _)  -> x == x' && y > y'
                                       (Bridge Left' _)  -> x < x' && y == y'
                                       (Bridge Right' _) -> x > x' && y == y'

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
bridgeAtPoint p i = asum . map bridgeAtPointFromIsland $ i
  where
    xMax = islandsMaxX i
    yMax = islandsMaxY i
    islandPoints = Set.fromList . map getIslandPoint $ i

    bridgeAtPointFromIsland :: Island -> Maybe Bridge
    bridgeAtPointFromIsland i = asum . Set.map (pointOnBridge p i) $ getIslandBridges i

    pointOnBridge :: Point -> Island -> Bridge -> Maybe Bridge
    pointOnBridge target island bridge
        | not (pointCouldExistOnBridge target island bridge) = Nothing
        | pointOnBridgeLoop startPoint target indPoint       = Just bridge
        | otherwise                                          = Nothing
      where
        incPoint = traverseBridge bridge
        startPoint = incPoint $ getIslandPoint island

    -- TODO could I make this a fold too?
    --foldl (\x acc -> acc || pointOnBridgeLoop (x targetP)) $ map traverseBridge startPoint
    -- TODO name no longer fits very well...
    pointOnBridgeLoop :: Point -> Point -> (Point -> Point) -> Bool
    pointOnBridgeLoop currentP targetP incPoint
        | currentP `Set.member` islandPoints = False
        | (getX currentP) > xMax = False
        | (getY currentP) > yMax = False
        | currentP == targetP    = True
        | otherwise              = pointOnBridgeLoop nextPoint targetP incPoint
      where
        nextPoint = incPoint currentP
{-
-- Combine eithers, with the end result being the first Left value found,
-- or the last Right value found
comb :: Either a b -> Either a b -> Either a b
comb (Left x) _ = Left x
comb _ (Left x) = Left x
comb e1 e2      = e2


-- TODO have validateIslands return islands if it is valid, and monads or fmap
--      to pass that to validateBridges (maybe?). Ex:
--
--        Left "Foo"  >>= (\x -> return "New")
--        (\x -> "New") <$> Right "Foo"
validatePuzzle :: [Island] -> [Bridge] -> Either String String
validatePuzzle i b = validateIslands i `comb` validateBridges b i `comb` Right "Puzzle is valid"


validateIslands :: [Island] -> Either String String
validateIslands islands
    | listLength < 2          = Left "Must have at least two islands in a puzzle"
    | listLength /= setLength = Left "Puzzle contains duplicate islands at the same point"
    | otherwise               = Right "Islands are valid"
    where points     = map (\ (Island p _) -> p) islands
          listLength = length points
          setLength  = length $ Set.fromList points


-- TODO use fold or filter here to check for errors instead of recursion,
--      need to keep the whole list so we can check for overlapping points.
--      PS. lets do that by making a list of all points between all bridges,
--      and converting it to a set. If the numbers don't match, there are
--      duplicates
validateBridges :: [Bridge] -> [Island] -> Either String String
validateBridges [] _ = Right "Bridges are valid"
validateBridges ((Bridge (Point x1 y1) (Point x2 y2) _):xs) islands
    | x1 == x2 && y1 == y2 = Left "Bridge must have two different points"
    | x1 /= x2 && y1 /= y2 = Left "Bridge must be horizontal or vertical"
    | otherwise            = validateBridges xs islands
-}

-- Valication includes:
--  * Making sure all bridges connect to another island
--  * Removing duplicate bridges?
--  * Make sure no bridges are intersecting
--
-- Checking for solved include:
--  * Making sure no bridges overlap
--  * Making sure all islands have teh correct number of bridges coming from them
--  * Making sure all islands are connected
