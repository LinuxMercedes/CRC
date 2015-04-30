import CRC (Bit(I,O), check)

main = do
  input <- getLine
  case check $ read input of
    Nothing -> print "CRC error"
    Just bits -> print bits

