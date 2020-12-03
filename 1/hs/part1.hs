import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Monad
import System.Exit

check :: Int -> Int -> IO ()
check a b = do
  when ((a + b) == 2020) $ do
    print $ a * b
    exitSuccess

main :: IO [[()]]
main = do
  text <- Text.readFile "list.txt"
  let list = map (read . Text.unpack) $ Text.lines text
  mapM (\x -> mapM (check x) list) list
