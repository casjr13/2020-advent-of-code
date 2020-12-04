import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Parselib

type PasswordPolicy = (Int, Int, Char, String)

line :: Parser PasswordPolicy
line = do
  first <- natural
  symb "-"
  second <- natural
  space
  c <- letter
  symb ":"
  password <- many letter
  space
  return (first, second, c, password)

parseLine :: String -> PasswordPolicy
parseLine l = case apply line l of
  [(a, "")] -> a
  _ -> error l

valid :: String -> Bool
valid l = do
  let (first, second, char, password) = parseLine l
  let a = char == (password !! (first - 1))
  let b = char == (password !! (second - 1))
  (a && not b) || (not a && b)

main = do
  text <- Text.readFile "passwords.txt"
  let list = map Text.unpack $ Text.lines text
  let fil = filter valid list
  let count = length fil
  print count
