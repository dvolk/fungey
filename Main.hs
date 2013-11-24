module Main where

import Control.Monad.State
import Data.Array.IArray
import Data.Char
import System.Random
import System.Environment
import System.IO

type Position = (Int, Int)
type Code = Array Position Char

data ProgMode = Normal | Slurp deriving Eq

data ProgState = ProgState
  { position :: Position
  , momentum :: Position -> Position
  , stack    :: [Int]
  , code     :: Code
  , mode     :: ProgMode
  }

stringToCode :: [String] -> Code
stringToCode ss =
  let by = length ss - 1
      bx = length (head ss) - 1
  in listArray ((0,0), (by, bx)) (concat ss)

left, right, up, down :: Position -> Position
left  (y, x) = (y, x - 1)
right (y, x) = (y, x + 1)
up    (y, x) = (y - 1, x)
down  (y, x) = (y + 1, x)

directions :: [Position -> Position]
directions = [left, right, up, down]

move :: StateT ProgState IO ()
move = do
  s <- get
  let -- code bounds
      (by, bx) = snd (bounds (code s))
      -- proposed new position
      (ny, nx) = momentum s (position s)
      -- wrap around if we're out of bounds
      ny' 
        | ny > by = 0
        | ny <  0 = by
        | otherwise = ny
      nx' 
        | nx > bx = 0
        | nx <  0 = bx
        | otherwise = nx

  modify $ \st -> st { position = (ny', nx') }

-- change momentum
collide :: (Position -> Position) -> StateT ProgState IO ()
collide f = modify $ \s -> s { momentum = f }

-- add value to stack
push :: Int -> StateT ProgState IO ()
push x = modify $ \s -> s { stack = x : stack s }

-- remove value from stack and return it
pop :: StateT ProgState IO Int
pop = do
  st <- gets stack
  if null st
    then return 0
    else do
      e <- head `fmap` gets stack
      modify $ \s -> s { stack = tail (stack s) }
      return e

toggleSlurpMode :: StateT ProgState IO ()
toggleSlurpMode =
  modify $ \s -> s { mode = if mode s == Normal then Slurp else Normal }

loop :: StateT ProgState IO ProgState
loop = do
  p <- gets position
  c <- gets code
  m <- gets mode
  let x = c ! p
--  liftIO $ putStrLn $ (show p) ++ " " ++ show sym
  if x == '@'
    -- exit with the current ProgState
    then get
    else do
     case () of
      _
       | x == '"' -> toggleSlurpMode
       | m == Slurp -> push (ord x)
       | x == '>' -> collide right
       | x == '<' -> collide left
       | x == '^' -> collide up
       | x == 'v' -> collide down
       | x == '#' -> move
       | x == '?' -> do
          i <- liftIO $ randomRIO (0,3)
          collide (directions !! i)
       | ord x >= 48 && ord x <= 57 -> push (read (return x))
       | x == '&' -> do
          n <- liftIO getLine
          push (read n)
       | x == '~' -> do
          n <- liftIO getChar
          push (ord n)
       -- operations that pop one element
       | x `elem` "!_|:$.," -> do
          a <- pop
          case x of
            '!' -> push    (if a == 0 then 1 else 0)
            '_' -> collide (if a == 0 then right else left)
            '|' -> collide (if a == 0 then down else up)
            ':' -> push a >> push a
            '$' -> return ()
            '.' -> liftIO $ putStr (show a)
            ',' -> liftIO $ putStr [chr a]
       -- operations that pop two elements
       | x `elem` "+*/-%`\\g" -> do
          a <- pop
          b <- pop
          case x of
            '+'  -> push (a + b)
            '*'  -> push (a * b)
            '-'  -> push (b - a)
            '/'  -> push (b `div` a)
            '%'  -> push (snd (b `divMod` a))
            '`'  -> push (if b > a then 1 else 0)
            '\\' -> push a >> push b
            'g'  -> push (ord (c ! (b, a)))
       | x == 'p' -> do
          a <- pop
          b <- pop
          v <- pop
          modify $ \s -> s { code = code s // [((b, a), chr v)] }
       | otherwise ->
          return ()
     move
     loop

main :: IO ()
main = do
  hSetBuffering  stdin NoBuffering
  hSetBuffering stdout NoBuffering
  
  as  <- getArgs
  sel <- lines `fmap` (if null as then getLine else readFile (head as))
  
  let initial =
       ProgState 
         { position = (0,0)
         , momentum = right
         , stack    = []
         , code     = stringToCode sel
         , mode     = Normal
         }
  
  r <- execStateT loop initial
  
  unless (null (stack r)) $
    putStrLn $ "Stack: " ++ show (stack r)
