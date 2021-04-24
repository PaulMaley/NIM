module RandomPlayer (getCompInput) where

import System.Random
import Data.List
import Types

-- Player state is encapsulated in RPlayer
getCompInput :: State -> RPlayer -> IO (Move, RPlayer)
getCompInput s p  = do
                   let l = filter ((/= 0) . snd) s
                       (r1, p') = randomR (0,(length l)-1) p
                       pile = fst (l !! r1)
                       size = snd (l !! r1) 
                       (n, p'') = randomR (1,size) p'
                   return ((pile,n), p'')  

