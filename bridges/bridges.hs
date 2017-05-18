import Data.List
import Data.Maybe
import Control.Applicative
import qualified Data.Set as Set

data Point = Point Int Int deriving (Eq, Show, Ord)
data IslandValue = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Show)
data BridgeValue = Single | Double deriving (Eq, Show)
data Island = Island Point IslandValue deriving (Eq, Show)
data Bridge = Bridge Point Point BridgeValue deriving (Eq, Show)


islandToChar :: Island -> Char
islandToChar (Island _ One)   = '1'
islandToChar (Island _ Two)   = '2'
islandToChar (Island _ Three) = '3'
islandToChar (Island _ Four)  = '4'
islandToChar (Island _ Five)  = '5'
islandToChar (Island _ Six)   = '6'
islandToChar (Island _ Seven) = '7'
islandToChar (Island _ Eight) = '8'


bridgeToChar :: Bridge -> Char
bridgeToChar (Bridge (Point x1 y1) (Point x2 y2) _)
    | x1 == x2 && y1 == y2 = error "Bridge must have two different points"
    | x1 /= x2 && y1 /= y2 = error "Bridge must be horizontal or vertical"
bridgeToChar (Bridge (Point x1 y1) (Point x2 y2) Single)
    | x1 == x2 = '|'
    | y1 == y2 = '―'
bridgeToChar (Bridge (Point x1 y1) (Point x2 y2) Double)
    | x1 == x2 = '‖'
    | y1 == y2 = '═'


pprint :: [Island] -> [Bridge] -> String
pprint islands bridges = pprintLoop 0 0 xMax yMax islands bridges
    where xMax = maximum . map (\(Island (Point x _) _) -> x) $ islands
          yMax = maximum . map (\(Island (Point _ y) _) -> y) $ islands


pprintLoop :: Int -> Int -> Int -> Int -> [Island] -> [Bridge] -> String
pprintLoop x y xMax yMax islands bridges
    | y > yMax  = ""
    | x > xMax  = "\n" ++ pprintLoop 0 (y+1) xMax yMax islands bridges
    | otherwise = c : " " ++ pprintLoop (x+1) y xMax yMax islands bridges
    where i = getIslandAtPoint (Point x y) islands
          b = getBridgeAtPoint (Point x y) bridges
          c = fromJust $ (islandToChar <$> i) <|> (bridgeToChar <$> b) <|> Just ' '


getIslandAtPoint :: Point -> [Island] -> Maybe Island
getIslandAtPoint p' = find (\(Island p _) -> p == p')


getBridgeAtPoint :: Point -> [Bridge] -> Maybe Bridge
getBridgeAtPoint p = find (pointBetweenBridge p)


pointBetweenBridge :: Point -> Bridge -> Bool
pointBetweenBridge (Point x' y') (Bridge (Point x1 y1) (Point x2 y2) _)
    | x1 == x2 && y1 == y2 = error "Bridge must have two different points"
    | x1 /= x2 && y1 /= y2 = error "Bridge must be horizontal or vertical"
    | x' == x1 && x' == x2 = min y1 y2 < y' && y' < max y1 y2
    | y' == y1 && y' == y2 = min x1 x2 < x' && x' < max x1 x2
    | otherwise            = False


-- Combine eithers, with the end result being the first Left value found,
-- or the last Right value found
comb :: Either a b -> Either a b -> Either a b
comb (Left x) _ = Left x
comb _ (Left x) = Left x
comb e1 e2             = e2


validatePuzzle :: [Island] -> [Bridge] -> Either String String
validatePuzzle i b = validIslands `comb` validBridges `comb` Right "Puzzle is valid"
    where validIslands = validateIslands i
          validBridges = Right "valid"


validateIslands :: [Island] -> Either String String
validateIslands i
    | listLength < 2          = Left "Must have at least two islands in a puzzle"
    | listLength /= setLength = Left "Puzzle contains duplicate islands at the same point"
    | otherwise               = Right "Islands are valid"
    where points = [Point 0 0, Point 0 1]  -- TODO
          listLength = length points
          setLength  = length $ Set.fromList points

-- Valication includes:
--  * There is at least one island
--  * Making sure all bridges are verticle
--  * Making sure all bridges start and end on an island
--
-- Checking for solved include:
--  * Making sure no bridges overlap
--  * Making sure all islands have teh correct number of bridges coming from them
--  * Making sure all islands are connected
