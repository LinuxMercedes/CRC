import CRC (Bit(I,O), generate)

main = do
  print . generate . read =<< getLine

