module Types (State, Move, RPlayer) where

import System.Random

type State = [(Char,Int)]
type Move = (Char,Int)
type RPlayer = StdGen 
 
