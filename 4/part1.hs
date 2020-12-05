import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Array.IO
import Data.List.Split
import System.Environment
import System.IO

type Passport = IOArray Criteria Bool
data Criteria = BYR | IYR | EYR | HGT | HCL | ECL | PID | CID 
  deriving (Show, Eq, Ord, Ix)
type Runner = ReaderT Passport IO

clearPassport :: Runner ()
clearPassport = clear $ range (BYR,CID)
  where 
    clear :: [Criteria] -> Runner ()
    clear [] = return ()
    clear (c:cs) = do
      p <- ask
      liftIO $ writeArray p c False
      clear cs

loop :: [Criteria] -> Runner Bool
loop [] = return True
loop (c:cs) = do
  p <- ask
  b <- liftIO $ readArray p c
  bs <- loop cs
  return $ b && bs

validPassport :: Runner Int
validPassport = do
  let cs = range (BYR,PID)
  b <- loop cs
  -- liftIO $ putStrLn $ if b then "Valid" else "Invalid"
  return $ if b then 1 else 0

getCriteria :: String -> Criteria
getCriteria kv = do
  let a = splitOn ":" kv
  case head a of
    "byr" -> BYR
    "iyr" -> IYR
    "eyr" -> EYR
    "hgt" -> HGT
    "hcl" -> HCL
    "ecl" -> ECL
    "pid" -> PID
    "cid" -> CID

setValid :: Criteria -> Runner ()
setValid c = do
  p <- ask
  liftIO $ writeArray p c True

parsePassport :: String -> Runner ()
parsePassport "" = return ()
parsePassport s = do
  let kvs = splitOn " " s
  let cs = map getCriteria kvs
  mapM_ setValid cs

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
  arr <- newArray (BYR, CID) False
  n <- runReaderT (parsePassports handle 0) arr
  hClose handle
  print n