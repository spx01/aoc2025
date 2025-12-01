module AOC where

import Text.Printf
import System.FilePath
import System.Environment
import System.IO
import Control.Arrow

type Interact = String -> String

inputPath :: Int -> FilePath
inputPath dayNum = "inputs" </> printf "day%02d.txt" dayNum

wrapInteract :: Int -> Interact -> IO ()
wrapInteract dayNum f = do
  args <- getArgs
  case args of
    ["-"] -> interact f >> putStrLn ""
    []    -> readFile' (inputPath dayNum) >>= putStrLn . f
    _     -> fail "invalid arguments"

combineParts :: Interact -> Interact -> Interact
combineParts f1 f2 s =
  concat ["part 1:\n\t", o1, "\npart 2:\n\t", o2]
  where
    (o1, o2) = f1 &&& f2 $ s
