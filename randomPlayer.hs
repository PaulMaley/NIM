module RandomPlayer (getCompInput) where

import System.Random
import Types

getCompInput :: State -> IO Move
getCompInput s = do
                   gen <- newStdGen
                   let l = map fst $ filter ((/= 0) . snd) s
                       pile = l !!  fst (randomR (0, (length l)-1) gen) 
                   return (pile,1)  
