import Data.List
import Data.Maybe
import Control.Applicative

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
    | y1 == y2 = '-'
bridgeToChar (Bridge (Point x1 y1) (Point x2 y2) Double)
    | x1 == x2 = '#'
    | y1 == y2 = '='


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

