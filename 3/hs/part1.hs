import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment

countTrees :: [[Char]] -> Int -> Int -> Int -> Int -> IO Int
countTrees m rows cols mx my = do
  loop 0 0 where
    loop :: Int -> Int -> IO Int
    loop i acc = do
      let x = (i * mx) `mod` (cols - 1)
      let y = i * my
      if y < rows
        then do
          let c = m !! y !! x
          let a = if c == '#' then 1 else 0
          loop (i + 1) (acc + a)
        else return acc

main = do
  args <- getArgs
  let mx = read $ head args
  let my = read $ head $ tail args
  text <- Text.readFile "map.txt"
  let m = map Text.unpack $ Text.lines text
  let rows = length m
  let cols = length (head m)
  trees <- countTrees m rows cols mx my
  print trees
