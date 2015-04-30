import CRC (Bit(I,O), alter)
import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of
    [] -> print "Please specify a position to flip"
    (n:_) -> print . (alter (read n :: Int)) . read =<< getLine

