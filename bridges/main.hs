import BridgeSolver
import Data.Maybe (fromJust)

fromRight :: Either a b -> b
fromRight (Right b) = b

testGame1 = fromRight $ createGame [(0, 0, 1), (2, 0, 1), (0, 2, 1), (2, 2, 1)]

testGame2 = fromRight $ createGame [(1, 0, 2), (5, 0, 4), (9, 0, 4), (0, 1, 1),
                                    (1, 2, 4), (4, 2, 3), (7, 2, 4), (9, 2, 5),
                                    (7, 4, 2), (5, 5, 4), (9, 5, 4), (1, 7, 1),
                                    (4, 7, 1), (7, 7, 1), (0, 8, 3), (5, 8, 4),
                                    (1, 9, 1), (3, 9, 2), (7, 9, 4), (9, 9, 4)]


-- TODO this is just a dummy main for testing stuff. What I want to do next
--      build another application on top of this which goes out to the
--      puzzle bridges website and downloads/solves games from there. All IO
--      will happen here.
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
