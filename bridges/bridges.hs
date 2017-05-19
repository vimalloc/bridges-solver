import Data.List
import Data.Maybe
import Control.Applicative
import qualified Data.Set as Set

-- Left' and Right' use the tick so they don't clash with Eithers Left or Right.
-- Up' and Down' use the ticks to stay consistant with Left' and Right'.
data BridgeValue = Single | Double deriving (Eq, Show, Ord)
data BridgeDirection = Up' | Down' | Left' | Right' deriving (Eq, Show, Ord)
data Bridge = Bridge BridgeDirection BridgeValue deriving (Eq, Show)

data Point = Point Int Int deriving (Eq, Show, Ord)
data IslandValue = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Show, Ord)
data Island = Island Point IslandValue (Set.Set Bridge) deriving (Eq, Show, Ord)

-- Define our own ordering test here. We are using a set of bridges for the
-- island, and we don't care if the bridge is a single or double there, only
-- what direction it is leaving the island from. This will insure we cannot
-- have duplicate bridges on an island without any addition checks on our part
instance Ord Bridge where
    (Bridge d1 _) `compare` (Bridge d2 _) = d1 `compare` d2


getX :: Point -> Int
getX (Point x _) = x

getY :: Point -> Int
getY (Point _ y) = y

getIslandPoint :: Island -> Point
getIslandPoint (Island p _ _) = p

getBridges :: Island -> (Set.Set Bridge)
getBridges (Island _ _ b) = b

getIslandX :: Island -> Int
getIslandX = getX . getIslandPoint

getIslandY :: Island -> Int
getIslandY = getY . getIslandPoint

getIslandPoints :: [Island] -> [Point]
getIslandPoints = map getIslandPoint

islandsMaxX :: [Island] -> Int
islandsMaxX = maximum . map getX . getIslandPoints

islandsMaxY :: [Island] -> Int
islandsMaxY = maximum . map getY . getIslandPoints

incPoint :: Bridge -> Point -> Point
incPoint (Bridge Up' _) (Point x y)    = (Point x (y-1))
incPoint (Bridge Down' _) (Point x y)  = (Point x (y+1))
incPoint (Bridge Left' _) (Point x y)  = (Point (x-1) y)
incPoint (Bridge Right' _) (Point x y) = (Point (x+1) y)


pointCouldExistOnBridge :: Point -> Island -> Bridge -> Bool
pointCouldExistOnBridge p i b = case b of
                                    (Bridge Up' _)    -> x == x' && y < y'
                                    (Bridge Down' _)  -> x == x' && y > y'
                                    (Bridge Left' _)  -> x < x' && y == y'
                                    (Bridge Right' _) -> x > x' && y == y'
    where x  = getX p
          y  = getY p
          x' = getIslandX i
          y' = getIslandY i



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
    where xMax = islandsMaxX islands
          yMax = islandsMaxY islands

          pprintLoop :: Int -> Int -> [Island] -> String
          pprintLoop x y islands
              | y > yMax  = ""
              | x > xMax  = "\n" ++ pprintLoop 0 (y+1) islands
              | otherwise = c : " " ++ pprintLoop (x+1) y islands
              where i = islandAtPoint (Point x y) islands
                    b = bridgeAtPoint (Point x y) islands
                    c = fromJust $ (islandToChar <$> i) <|> (bridgeToChar <$> b) <|> Just ' '


islandAtPoint :: Point -> [Island] -> Maybe Island
islandAtPoint p = find (\ i -> getIslandPoint i == p)


-- I dont really like this function. Currently I'm using nested functions so
-- that I don't have to pass xMax, yMax, and islandPoints all over place
-- (or re-cacluate them unnecessarily). I dunno, maybe this is fine, but
-- something about it rubs me the wrong way... One idea I had was instead
-- of passing the bridge to pointOnBridgeLoop, I could just pass a clouse of
-- the incPoint function that already had the direction in it? I dunno, maybe
-- that would actually make it more confusing. Think on it.
bridgeAtPoint :: Point -> [Island] -> Maybe Bridge
bridgeAtPoint p i = foldl (\acc x -> acc <|> (bridgeAtPointFromIsland x)) Nothing i
    where xMax = islandsMaxX i
          yMax = islandsMaxY i
          islandPoints = Set.fromList $ map (\ (Island p _ _) -> p) i

          bridgeAtPointFromIsland :: Island -> Maybe Bridge
          bridgeAtPointFromIsland i = foldl (\acc b -> acc <|> pointOnBridge p i b) Nothing bridges
              where bridges = getBridges i

          -- Shortcircuit the computation if the point couldn't possible be on this bridge
          pointOnBridge :: Point -> Island -> Bridge -> Maybe Bridge
          pointOnBridge target island bridge
              | (pointCouldExistOnBridge target island bridge) == False = Nothing
              | otherwise = pointOnBridgeLoop startPoint target bridge
              where startPoint = incPoint bridge (getIslandPoint island)

          pointOnBridgeLoop :: Point -> Point -> Bridge -> Maybe Bridge
          pointOnBridgeLoop currentP targetP bridge
              | currentP `Set.member` islandPoints = Nothing
              | (getX currentP) > xMax = Nothing
              | (getY currentP) > yMax = Nothing
              | currentP == targetP    = Just bridge
              | otherwise              = pointOnBridgeLoop (incPoint bridge currentP) targetP bridge
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
