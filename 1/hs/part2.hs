import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Monad
import System.Exit

check :: Int -> Int -> Int -> IO ()
check a b c = do
  when ((a + b + c) == 2020) $ do
    print $ a * b * c
    exitSuccess

main :: IO [[[()]]]
main = do
  text <- Text.readFile "list.txt"
  let list = map (read . Text.unpack) $ Text.lines text
  mapM (\x -> mapM (\y -> mapM (check x y) list) list) list
