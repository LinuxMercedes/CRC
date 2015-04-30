module CRC (Bit(O,I), crc, crccheck, ieeepoly, generate, check, alter) where

import Data.Maybe

data Bit = O | I deriving Eq

_read '0' = O
_read '1' = I

instance Show Bit where
  show O = "0"
  show I = "1"

  showList bits = showString $ foldr (\b l -> (show b) ++ l) "" bits

instance Read Bit where
  readsPrec _ (bit:input) = [(_read bit, input)]
  readList input = [(map _read input, ""::String)]

-- Polynomial algebra
trunc :: [Bit] -> [Bit]
trunc [] = []
trunc (x:xs) 
  | x == O = trunc xs
  | otherwise = x:xs

sub :: [Bit] -> [Bit] -> [Bit]
sub x y = zipWith _sub x y ++ drop (length y) x
  where
    _sub :: Bit -> Bit -> Bit
    _sub O O = O
    _sub I O = I
    _sub O I = I
    _sub I I = O

remainder :: [Bit] -> [Bit] -> [Bit]
remainder dividend divisor
  | length divisor > length dividend = dividend
  | head dividend == O = remainder(tail dividend) divisor
  | otherwise = remainder (tail $ sub dividend divisor) divisor

pad :: Int -> [Bit] -> [Bit]
pad n xs
  | length xs >= n = xs
  | otherwise = pad n $ xs ++ [O]

-- Generic CRC stuff
crc :: [Bit] -> [Bit] -> [Bit]
crc polynomial val = val ++ remainder (pad (length polynomial + length val - 1) val) polynomial

crccheck :: [Bit] -> [Bit] -> Maybe [Bit]
crccheck polynomial val 
  | 0 == (length . trunc $ remainder val polynomial) = Just $ take (length val - length polynomial + 1) val
  | otherwise = Nothing

-- 100000100110000010001110110110111
-- IEEE 802.3 generator polynomial
ieeepoly = [I,O,O,O,O,O,I,O,O,I,I,O,O,O,O,O,I,O,O,O,I,I,I,O,I,I,O,I,I,O,I,I,I]

-- IEEE CRC
generate :: [Bit] -> [Bit]
generate = crc ieeepoly

check :: [Bit] -> Maybe [Bit]
check = crccheck ieeepoly

-- Bit flip generator
neg :: Bit -> Bit
neg O = I
neg I = O

alter :: Int -> [Bit] -> [Bit]
alter n [] = []
alter 0 (x:xs) = (neg x) : xs
alter n (x:xs) = x : alter (n-1) xs
