{--
Text based NIM game
state is of form [('A',1), ('B',2), ...]
--}

import System.Environment 
import System.Exit
import Data.Char
import Data.List
import System.Random

type State = [(Char,Int)]
type Move = (Char,Int)

main = do 
         args <- getArgs
         nstr <- parse args
         gen <- getStdGen
         loop (setup (read nstr :: Int))  

loop :: State -> IO State
loop state = do
               putStrLn (display state) 
               choice <- getInput
               let state' = move state choice 
               putStrLn (display state')        -- State after player's move
               if (gameOver state') 
               then do
                 putStrLn "Computor wins!"
                 return state
               else do 
                 choice' <- comp state'
                 let state'' = move state' choice' -- Computor moves 
                 if (gameOver state'')
                 then do
                   putStrLn "You win!" -- Computor took last object
                   return state''
                 else do
                   loop state''

setup :: Int -> State
setup n = zip "ABCDEFG" [1..n]

gameOver :: State -> Bool
gameOver s = all ((== 0) . snd) s

-- nicer display of state
display :: State -> String
display s = foldr ((++) . f) "" s
            where
              f e = (fst e): ": " ++ (replicate (snd e) '*') ++ "  "

-- Game mechanics (no error checking !)
move :: State -> Move -> State
move s m = map (\(c,n) -> if (c /= (fst m)) 
                          then (c,n) 
                          else (c,n-(snd m))) s

-- Computor move (takes one off a random pile)
comp :: State -> IO Move
comp s = do
           gen <- newStdGen
           let l = map fst $ filter ((/= 0) . snd) s
               pile = l !!  fst (randomR (0, (length l)-1) gen) 
           return (pile,1)     

getInput :: IO Move
getInput = do
             putStrLn "Enter move"
             input <- getLine
             let c:n:[] = words input
             return (toUpper (head c), read n :: Int)

parse :: [String] -> IO String 
parse ["-v"] = (putStrLn version) >> exit
parse ["-h"] = putStrLn usage >> exit
parse [nstr] = return nstr

version :: String
version = "Nim v1.0 beta"

usage :: String
usage = "nim n (where n is a natural number)"

exit = exitWith ExitSuccess
