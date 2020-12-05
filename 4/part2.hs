import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Array.IO
import Data.Char
import Data.List.Split
import Parselib
import System.Environment
import System.IO

type Passport = IOArray Criteria (Maybe String)
data Criteria = BYR | IYR | EYR | HGT | HCL | ECL | PID | CID | UNKNOWN
  deriving (Show, Eq, Ord, Ix)
type Runner = ReaderT Passport IO
data Dimension = CM Int | IN Int

clearPassport :: Runner ()
clearPassport = clear $ range (BYR,CID)
  where 
    clear :: [Criteria] -> Runner ()
    clear [] = return ()
    clear (c:cs) = do
      p <- ask
      liftIO $ writeArray p c Nothing
      clear cs

parseIN :: Parser Dimension
parseIN = do
  n <- natural
  symb "in"
  return $ IN n

parseCM :: Parser Dimension
parseCM = do
  n <- natural
  symb "cm"
  return $ CM n

parseDim :: Parser Dimension
parseDim = parseIN +++ parseCM

isHex :: Char -> Bool
isHex c = c `elem` "0123456789abcdef"

isHexColor :: String -> Bool
isHexColor s = colorLoop s 0 where
  colorLoop "" 6 = True
  colorLoop "" _ = False
  colorLoop (c:cs) n = isHex c && colorLoop cs (n + 1)

isPID :: String -> Bool
isPID s = pidLoop s 0 where
  pidLoop "" 9 = True
  pidLoop "" _ = False
  pidLoop (c:cs) n = isDigit c && pidLoop cs (n + 1)

validate :: Criteria -> Maybe String -> Bool
validate BYR (Just v) = 1920 <= n && n <= 2002 where n = read v
validate IYR (Just v) = 2010 <= n && n <= 2020 where n = read v
validate EYR (Just v) = 2020 <= n && n <= 2030 where n = read v
validate HGT (Just v) = case apply parseDim v of
  [(d,"")] -> case d of
    (CM n) -> 150 <= n && n <= 193
    (IN n) -> 59 <= n && n <= 76
  _ -> False
validate HCL (Just v) = head v == '#' && isHexColor (tail v)
validate ECL (Just v) = v `elem` ["amb", "blu", "brn" , "gry", "grn", "hzl", "oth"]
validate PID (Just v) = isPID v
validate CID (Just v) = True
validate UNKNOWN (Just _) = False
validate c Nothing = c == CID

loop :: [Criteria] -> Runner Bool
loop [] = return True
loop (c:cs) = do
  p <- ask
  mv <- liftIO $ readArray p c
  bs <- loop cs
  let b = validate c mv
  -- liftIO $ putStrLn $ show c ++ " : " ++ show mv ++ " => " ++ show b
  return $ b && bs

validPassport :: Runner Int
validPassport = do
  let cs = range (BYR,PID)
  b <- loop cs
  -- liftIO $ putStrLn $ if b then "Valid" else "Invalid"
  return $ if b then 1 else 0

getCriteria :: String -> (Criteria, String)
getCriteria kv = (c, v) where
  a = splitOn ":" kv
  v = head $ tail a
  c = case head a of
    "byr" -> BYR
    "iyr" -> IYR
    "eyr" -> EYR
    "hgt" -> HGT
    "hcl" -> HCL
    "ecl" -> ECL
    "pid" -> PID
    "cid" -> CID
    _ -> UNKNOWN

setValue :: (Criteria, String) -> Runner ()
setValue (c,v) = do
  p <- ask
  liftIO $ writeArray p c (Just v)

parsePassport :: String -> Runner ()
parsePassport "" = return ()
parsePassport s = do
  let kvs = splitOn " " s
  let cvs = map getCriteria kvs
  mapM_ setValue cvs

continue :: Handle -> Int -> Runner Int
continue handle count = do
  valid <- validPassport
  clearPassport
  parsePassports handle $ count + valid

parsePassports :: Handle -> Int -> Runner Int
parsePassports handle count = do
  eof <- liftIO $ hIsEOF handle
  if eof
    then do 
      valid <- validPassport
      return $ count + valid
    else do
      s <- liftIO $ hGetLine handle
      case s of
        "" -> continue handle count
        _    -> do
          parsePassport s
          parsePassports handle count

main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  arr <- newArray (BYR, UNKNOWN) Nothing
  n <- runReaderT (parsePassports handle 0) arr
  hClose handle
  print n