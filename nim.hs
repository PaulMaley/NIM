{--
Text based NIM game
state is of form [('A',1), ('B',2), ...]
--}

import System.Environment 
import System.Exit
import Data.Char
import Data.List
import System.Random

import Types
import RandomPlayer
--type State = [(Char,Int)]
--type Move = (Char,Int)

main = do 
         args <- getArgs
         nstr <- parse args
         rplayer <- getStdGen
         loop rplayer (setup (read nstr :: Int))  

loop :: RPlayer -> State -> IO State
loop rp state = do
               putStrLn (display state) 
               playerMove <- getValidInput state
               let state' = move state playerMove 
               putStrLn (display state')        -- State after player's move
               if (gameOver state') 
               then do
                 putStrLn "Computor wins!"
                 return state
               else do 
                 (computorMove,rp') <- getCompInput state' rp
                 let state'' = move state' computorMove 
                 if (gameOver state'')
                 then do
                   putStrLn "You win!" -- Computor took last object
                   return state''
                 else do
                   loop rp' state''

setup :: Int -> State
setup n = zip "ABCDEFG" [1..n]

gameOver :: State -> Bool
gameOver s = all ((== 0) . snd) s

-- function to determine whether a move is valid
validQ :: State -> Move -> Bool
validQ s (_,0) = False -- At least one thing must be removed !!
validQ s (c,n) = any (\(c',n') -> ((c==c') && (n<=n'))) s

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

-- Get a player move from std in. Try to accept anything 
-- of the form "a2" "A 2" "A2" .. that can be understood
-- as a letter and a number. The validity of a move (function
-- of the state) is determined elsewhere.

getValidInput :: State -> IO Move
getValidInput state = do
                        move <- getInput
                        if validQ state move then
                          return move
                        else
                          getValidInput state 

getInput :: IO Move
getInput = do
             putStrLn "Enter move"
             input <- getLine
             let m = parseInput input in case m of
               Nothing -> getInput
               Just v  -> return v  

parseInput :: String -> Maybe Move
parseInput str = let str' = (filter isAlphaNum str) in
                 case str' of 
                   c:[] -> Nothing 
                   c:n:_ -> if (isLetter c) && (isDigit n) then 
                              Just (toUpper c, digitToInt n)
                            else 
                              Nothing 
  
   
            
parse :: [String] -> IO String 
parse ["-v"] = (putStrLn version) >> exit
parse ["-h"] = putStrLn usage >> exit
parse [nstr] = return nstr

version :: String
version = "Nim v1.0 beta"

usage :: String
usage = "nim n (where n is a natural number < 10)"

exit = exitWith ExitSuccess
