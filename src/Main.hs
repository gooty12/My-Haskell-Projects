{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import System.Random
import Data.Aeson (encode, eitherDecode, FromJSON, ToJSON)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as DB
import Data.List
import Data.Array
import Data.Maybe
import System.IO

newtype GameSetupInfo = GameSetupInfo [[[Int]]] deriving (Show, Generic)
gameSetupInfoToList (GameSetupInfo ls) = ls

instance ToJSON GameSetupInfo
instance FromJSON GameSetupInfo

data GameState = 
  GameState {turn :: Int,
             players :: [[[Int]]], 
             spaces :: [[Int]]} deriving (Show, Generic)

instance FromJSON GameState
instance ToJSON GameState

data Tokens = Tokens {token1 :: (Int, Int), token2 :: (Int, Int)} deriving Eq

type Board = Array (Int, Int) Int

-- Given a list of lists ([[Int]]), mkBoard converts it to 2-d array
mkBoard ls = 
  array ((1,1), (rows, cols)) 
        [(i, e) | (i, e) <- zip  [(i, j) | i <- [1..rows], j <- [1..cols]] (concat ls)]

boardToList board = 
  [elem | l <- ls , let elem = snd $ unzip l] 
    where ls = groupBy (\a b -> (fst $ fst a) == (fst $ fst b)) (assocs board)

rows = 5
cols = 5
maxLevel= 4
winLevel = 3

-- Helper function to convert an index of (Int, Int) to an Int 
-- ranging from 0..rows*cols-1
intToIndex :: Int -> Int -> (Int, Int)
intToIndex n base = let (rw, col) = (n `quot` cols, n `mod` cols)
                    in 
                      if base==0 then (rw, col) else (rw+1, col+1)

indexToList :: (Int, Int) -> [Int]
indexToList indx = [fst indx, snd indx]

listToIndex :: [Int] -> (Int, Int)
listToIndex l = (head l, last l)

main :: IO ()
main = do  
  str <- getLine
  let gameInfo = (eitherDecode $ DB.pack $ str) :: Either String GameSetupInfo
  case gameInfo of
    Left err -> putStrLn err
    Right info -> do
                      g <- newStdGen
                      let  playerTokens =  gameSetupInfoToList info
                           availableIndxs = [[rw,col] | rw <- [1..rows], col <- [1..cols]] \\  (concat playerTokens)
                           (indx1, g2) = randomR (0, length availableIndxs - 1) g
                           (indx2, _) = randomR (0, (length availableIndxs) - 2) g2
                           pos1 = availableIndxs !! indx1
                           pos2 = (delete pos1 availableIndxs !! indx2) :: [Int]
                           allTokens = playerTokens ++ [(pos1 : [pos2])]
                      print allTokens
                      hFlush stdout
                      playGame ()
                          
  return ()

isWinner board tokens = (board ! (token1 tokens))== winLevel ||
                        (board ! (token2 tokens))== winLevel
                        
playGame () = do 
  str <- getLine
  let state = (eitherDecode $ DB.pack $ str) :: Either String GameState
  case state of
    Left err -> putStrLn err
    Right st -> do 
                  let p1Tokens = getTokens $ head $ players $ st
                      p2Tokens = getTokens $ last $ players $ st
                      getTokens ls = Tokens (listToIndex $ head $ ls) (listToIndex $ last $ ls)
                      (board, nwTokens) = play (mkBoard $ spaces st) p1Tokens p2Tokens
                  let p1TokensList = [indexToList $ token1 nwTokens, indexToList $ token2 nwTokens]
                  putStrLn $ DB.unpack $ encode $ GameState ((turn st) + 1) [last $ players st, p1TokensList] (boardToList board)
                  hFlush stdout
                  if (isWinner board nwTokens) || (nwTokens == p1Tokens) then
                    return ()
                  else 
                    playGame ()
                  

play board p1Tokens p2Tokens = if score1 >= score2 then 
                                (build board1 nwTokens1 p1Tokens p2Tokens, nwTokens1)
                               else 
                                (build board2 nwTokens2 p1Tokens p2Tokens, nwTokens2)
                               where (board1, nwTokens1, score1) = bestMove board (token1 p1Tokens) p1Tokens p2Tokens
                                     (board2, nwTokens2, score2) = bestMove board (token2 p1Tokens) p1Tokens p2Tokens

-- checks that the given position is not one of the players' tokens                                    
notAToken pos p1Tokens p2Tokens = (pos /= (token1 p1Tokens)) && (pos /= (token2 p1Tokens)) && 
                                  (pos /= (token1 p2Tokens)) && (pos /= (token2 p2Tokens))                                     

-- checks if the two positions are adjacent (one of the 8 surrounding positions)
areAdjacent pos1 pos2 = (pos1 /= pos2) && dy <= 1 && dx <= 1 where dx = abs (fst pos1 - fst pos2); dy = abs (snd pos1 - snd pos2)

-- the first token in nwTokens is the new position we moved into
bestMove board token p1Tokens p2Tokens = 
  let boardMap = assocs board
      validMap = filter isValid boardMap
      isValid pair =  (notAToken pos p1Tokens p2Tokens) && (areAdjacent pos token) &&
                      (val /= maxLevel) && ((val == curVal+1) || (val <= curVal))
                       where curVal = board ! token; (pos, val) = pair
      (nwpos, score) = foldl (\a b -> if (snd b) > (snd a) then b else a) (token, -1) validMap
      nwTokens = if nwpos==token then 
                  p1Tokens
                 else
                  Tokens nwpos (if token == (token1 p1Tokens) then token2 p1Tokens else token1 p1Tokens)
  in 
    (board, nwTokens, score)

    
-- the first token in nwTokens is the position adjacent to which we build   
build board nwTokens p1Tokens p2Tokens =
    if (isWinner board nwTokens) || (nwTokens == p1Tokens) then
      board
    else
      let id elem = True
          isBlocking pair = isNextLevel pair p2Tokens
          isJump pair = isNextLevel pair nwTokens
          isNextLevel pair tokens = (val == val1 + 1) || (val == val2+1)
                                    where val = snd pair;
                                          val1 = board ! (token1 tokens)
                                          val2 = board ! (token2 tokens)
          boardMap = assocs board
          validMap = filter isValid boardMap
          token = token1 nwTokens
          isValid pair =  (notAToken pos nwTokens p2Tokens) && (areAdjacent pos token) &&
                          (val /= maxLevel) where (pos, val) = pair
          (pos, curVal) = fromJust (foldl (\p f -> if (isNothing p) then (find f validMap) else p) Nothing [isBlocking, isJump, id])
          nwAssoc = (pos, curVal+1)
      in
        board // [nwAssoc]    
                      
