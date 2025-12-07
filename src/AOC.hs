module AOC (wrapInteract, combineParts) where

import Text.Printf
import System.FilePath
import System.Environment
import System.IO
import Control.Arrow
import System.Clock
import Data.Ratio

type Interact = String -> String

inputPath :: Int -> FilePath
inputPath dayNum = "inputs" </> printf "day%02d.txt" dayNum

nsToMs :: Integer -> Double
nsToMs n = fromRational $ n % 1_000_000

printTime :: IO a -> IO a
printTime act = do
  start <- getTime Monotonic
  a <- act
  end <- getTime Monotonic
  let dt = toNanoSecs $ diffTimeSpec end start
  printf "time: %.03fms\n" (nsToMs dt)
  pure a

wrapInteract :: Int -> Interact -> IO ()
wrapInteract dayNum f = do
  args <- getArgs
  case args of
    ["-"] -> printTime $ interact f >> putStrLn ""
    []    -> readFile' (inputPath dayNum) >>= printTime . putStrLn . f
    _     -> fail "invalid arguments"

combineParts :: Interact -> Interact -> Interact
combineParts f1 f2 s =
  concat ["part 1:\n\t", o1, "\npart 2:\n\t", o2]
  where
    (o1, o2) = f1 &&& f2 $ s
