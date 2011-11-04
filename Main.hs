module Main where

import Text.RJson
import System.IO
import qualified Data.Map as M
import Data.List
import Data.Eq
import Data.Ord
import Debug.Trace

testFile = readFile "23885.txt"

allObjectAsMap = do
    f <- testFile
    case parseJsonString f of
        Right (JDObject dta) -> do
            return dta
        _ -> error "wrong format?"

fromJSObject (JDObject m) = m
fromJSObject v = error $ "fromJSObject: " ++ (show v)

fromJSString (JDString s) = s
fromJSString v = error $ "fromJSString: " ++ (show v)

fromJSInt (JDNumber s) = round s
fromJSInt v = error $ "fromJSInt: " ++ (show v)

fromJSDouble (JDNumber s) = s
fromJSDouble v = error $ "fromJSInt: " ++ (show v)

fromJSArray (JDArray a) = a
fromJSArray v =error $ "fromJSArray: " ++ (show v) 

data Ant = Ant  {
            antX :: Int,
            antY :: Int,
            antStart :: Int,
            antEnd :: Int,
            antPlayer :: Int,
            antMoves :: String
        } 

instance Show Ant where
    show a = "Ant " ++ (show $ antX a) ++ " " ++ (show $ antY a) ++ " " ++ (show $ antStart a) ++ " " ++ (show $ antEnd a)

data Game = Game {
                gameAttackRadius :: Double,
                gameViewRadius :: Double,
                gameMapSize :: (Int,Int),
                gameAnts :: [Ant],
                gamePlayers :: Int
            } deriving (Show)

data GameInProgress = GameInProgress {
            gipGame :: Game,
            gipNextTurn :: Int,
            gipAntsToBe :: [Ant],
            gipAntsAlive :: [Ant]
        }
        

loadGame = do
    v <- allObjectAsMap
    let replayData = fromJSObject $ v M.! "replaydata"
    let ants0= fromJSArray $ replayData M.! "ants"
    let ants = map (\(JDArray [x,y,s,e,p,c]) -> Ant (fromJSInt x) (fromJSInt y) 
                                                    (fromJSInt s) (fromJSInt e) (fromJSInt p)
                                                    (fromJSString c)) ants0
    let attackRadius = fromJSDouble $ replayData M.! "attackradius2"
    let viewRadius = fromJSDouble $ replayData M.! "viewradius2"
    let mapp = fromJSObject $ replayData M.! "map"
    let nplayers = length $ fromJSArray $ v M.! "playernames"
    let game = Game attackRadius viewRadius (fromJSInt $ mapp M.! "cols", fromJSInt $ mapp M.! "rows") ants nplayers
    return game

tracen a b =
    trace (show a) b

startGame g =
    let (antsToGiveBirth,antsToKeep) = span (\a -> antStart a == 0) (gameAnts g)
    in GameInProgress g 0 (sortBy (comparing antStart) $ antsToKeep) antsToGiveBirth

spanx cond xs = spanx' xs ([],[])
    where spanx' [] (as,bs) = (reverse as, reverse bs)
          spanx' (x:xs) (as,bs) = if cond x then spanx' xs (x:as,bs) else spanx' xs (as,x:bs)

advanceGame gip = 
    let (antsToGiveBirth,antsToKeep) = span (\a -> antStart a == gipNextTurn gip) (gipAntsToBe gip)
        (antsToKeep2, antsToDie) = spanx (\a -> antEnd a > gipNextTurn gip) (gipAntsAlive gip)
    in tracen ("antsToDie: ",antsToDie) $ 
            gip { gipNextTurn = gipNextTurn gip + 1,
             gipAntsToBe = antsToKeep,
             gipAntsAlive = antsToGiveBirth ++ map (forwardAnt $ gipGame gip) antsToKeep2 }

forwardAnt _ a@(Ant _ _ _ _ _ []) = a
forwardAnt g a@(Ant x y s e p (c:cs)) = 
    let (w,h) = gameMapSize g
    in case c of
        'e' -> Ant ((x - 1 + w) `mod` w) y s e p cs
        'w' -> Ant ((x + 1) `mod` w) y s e p cs
        'n' -> Ant x ((y - 1 + h) `mod` h) s e p cs
        's' -> Ant x ((y + 1) `mod` h) s e p cs
        _ -> a

gameStatus gip = "GameStatus " ++ (show $ gipNextTurn gip) ++ " " ++ 
                    (show $ map length $ groupBy (\a b -> antPlayer a == antPlayer b) 
                        $ sortBy (comparing antPlayer) (gipAntsAlive gip))


makeTurns gp
    | null $ gipAntsAlive gp = return ()
    | otherwise = do
        putStrLn $ gameStatus gp
        makeTurns $ advanceGame gp
        

main = do
    g <- loadGame
    print $ gameMapSize g
    let gp = startGame g
    putStrLn $ gameStatus gp
    makeTurns gp
    return ()
    
