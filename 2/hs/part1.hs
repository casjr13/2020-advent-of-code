import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Parselib

type PasswordPolicy = (Int, Int, Char, String)

line :: Parser PasswordPolicy
line = do
  min <- natural
  symb "-"
  max <- natural
  space
  c <- letter
  symb ":"
  password <- many letter
  space
  return (min, max, c, password)

parseLine :: String -> PasswordPolicy
parseLine l = case apply line l of
  [(a, "")] -> a
  _ -> error l

valid :: String -> Bool
valid l = do
  let (min, max, char, password) = parseLine l
  let fil = filter (== char) password
  let count = length fil
  min <= count && count <= max

main = do
  text <- Text.readFile "passwords.txt"
  let list = map Text.unpack $ Text.lines text
  let fil = filter valid list
  let count = length fil
  print count
