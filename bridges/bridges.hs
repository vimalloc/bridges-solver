import Data.List
import Data.Maybe
import Control.Applicative
import qualified Data.Set as Set

-- Left' and Right' use the tick so they don't clash with Eithers Left or Right.
-- Up' and Down' use the ticks to stay consistant with Left' and Right'.
data BridgeValue = Single | Double deriving (Eq, Show, Ord)
data BridgeDirection = Up' | Down' | Left' | Right' deriving (Eq, Show, Ord)
data Bridge = Bridge BridgeDirection BridgeValue deriving (Show, Ord)

data Point = Point Int Int deriving (Eq, Show, Ord)
data IslandValue = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Show, Ord)
data Island = Island Point IslandValue (Set.Set Bridge) deriving (Eq, Show, Ord)

-- Define our own equality test here. We are using a set of bridges for the
-- island, and we don't care if the bridge is a single or double there, only
-- what direction it is leaving the island from. This will insure we cannot
-- have duplicate bridges on an island without any runtime checks
instance Eq Bridge where
    (Up' _) == (Up' _)       = True
    (Down' _) == (Down' _)   = True
    (Left' _) == (Left' _)   = True
    (Right' _) == (Right' _) = True
    _ == _                   = False

-- TODO any way I could encode no equilviant bridges in here? Ie, two bridges
--      that start connect the same two islands, but start at different islands?

-- TODO add bridges as data inside the island
--      * This will insure at the type system level that all bridges are attached to an island
--      * How to prevent duplicate bridges? Sets instead of list? Would need to update the
--        Eq deriving to have it just be the same location instead of location and value,
--        but this would allow me to guarentee to duplicates, again at the type system
--      * Neec to figure out best way to check both bridges coming into and going
--        out of an island when I'm solving it. This still seems like the big setback,
--        but the other benefits seem like it would probably make this hassle worth it


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
bridgeToChar (Bridge Up' Single)    = '|'
bridgeToChar (Bridge Down' Single)  = '|'
bridgeToChar (Bridge Up' Double)    = '‖'
bridgeToChar (Bridge Down' Double)  = '‖'
bridgeToChar (Bridge Left' Single)  = '―'
bridgeToChar (Bridge Right' Single) = '―'
bridgeToChar (Bridge Left' Double)  = '═'
bridgeToChar (Bridge Right' Double) = '═'


pprint :: [Island] -> String
pprint islands = pprintLoop 0 0 xMax yMax islands
    where xMax = maximum . map (\(Island (Point x _) _ _) -> x) $ islands
          yMax = maximum . map (\(Island (Point _ y) _ _) -> y) $ islands


pprintLoop :: Int -> Int -> Int -> Int -> [Island] -> String
pprintLoop x y xMax yMax islands bridges
    | y > yMax  = ""
    | x > xMax  = "\n" ++ pprintLoop 0 (y+1) xMax yMax islands
    | otherwise = c : " " ++ pprintLoop (x+1) y xMax yMax islands
    where i = islandAtPoint (Point x y) islands
          b = bridgeAtPoint (Point x y) islands
          c = fromJust $ (islandToChar <$> i) <|> (bridgeToChar <$> b) <|> Just ' '


islandAtPoint :: Point -> [Island] -> Maybe Island
islandAtPoint p' = find (\(Island p _) -> p == p')


-- TODO pull bridge out of result somehow...
bridgeAtPoint :: Point -> [Island] -> Maybe Bridge
bridgeAtPoint p i = find (pointBetweenBridge p i) i


-- TODO actually see if the point exists in this bridge
pointBetweenBridge :: Point -> [Island] -> Island -> Bool
pointBetweenBridge (Point x1 y1) _ (Island (Point x2 y2) _ (Set.Set Bridge Up _))
    | x1 == x2 && y1 < y2 = True
    | otherwise           = False
pointBetweenBridge (Point x1 y1) _ (Island (Point x2 y2) _ (Set.Set Bridge Down _))
    | x1 == x2 && y1 > y2 = True
    | otherwise           = False
pointBetweenBridge (Point x1 y1) _ (Island (Point x2 y2) _ (Set.Set Bridge Left _))
    | x1 < x2 && y1 == y2 = True
    | otherwise           = False
pointBetweenBridge (Point x1 y1) _ (Island (Point x2 y2) _ (Set.Set Bridge Right _))
    | x1 > x2 && y1 == y2 = True
    | otherwise           = False


--pointBetweenBridge point islands = point `elem` bridgePoints
--    where bridgePoints = pointsInBridge bridge islands


-- Remember, we are says Pont 0 0 is the top left point of the puzzle,
-- thus up and down here seem reversed
pointsInBridge :: Bridge -> [Island] -> [Point]
pointsInBridge b i = case b of
                         (Bridge (Point x y) Up' _)    -> pointsInBridgeLoop (Point x (y-1)) Up' island_points
                         (Bridge (Point x y) Down' _)  -> pointsInBridgeLoop (Point x (y+1)) Down' island_points
                         (Bridge (Point x y) Left' _)  -> pointsInBridgeLoop (Point (x-1) y) Left' island_points
                         (Bridge (Point x y) Right' _) -> pointsInBridgeLoop (Point (x+1) y) Right' island_points
    where island_points = Set.fromList $ map (\ (Island p _) -> p) i
          xMax          = maximum . map (\(Island (Point x _) _) -> x) $ i
          yMax          = maximum . map (\(Island (Point _ y) _) -> y) $ i


-- TODO account for possible infinate loop here. Pass in xmax and ymax and verify
--      the bridge doesn't go past either of those two points
pointsInBridgeLoop :: Point -> BridgeDirection -> Set.Set Point -> [Point]
pointsInBridgeLoop p _ island_points
    | p `Set.member` island_points = []
pointsInBridgeLoop (Point x y) Up' i    = (Point x y) : pointsInBridgeLoop (Point x (y-1)) Up' i
pointsInBridgeLoop (Point x y) Down' i  = (Point x y) : pointsInBridgeLoop (Point x (y+1)) Down' i
pointsInBridgeLoop (Point x y) Left' i  = (Point x y) : pointsInBridgeLoop (Point (x-1) y) Left' i
pointsInBridgeLoop (Point x y) Right' i = (Point x y) : pointsInBridgeLoop (Point (x+1) y) Right' i


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
--  * Making sure all bridges start on an island and end on another island
--  * Make sure no bridges are intersecting
--
-- Checking for solved include:
--  * Making sure no bridges overlap
--  * Making sure all islands have teh correct number of bridges coming from them
--  * Making sure all islands are connected
