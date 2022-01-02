type Peg = String
type Move = (Peg, Peg)

-- Hanoi with 3 pegs
-- Returns the moves required to go from peg1 to peg2
hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 n peg1 peg2 peg3
  | n <= 0    = []
  | otherwise = hanoi3 (n-1) peg1 peg3 peg2 ++
                [(peg1,peg2)] ++
                hanoi3 (n-1) peg3 peg2 peg1

-- Hanoi with 4 pegs
-- Returns the moves required to go from peg1 to peg2
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n peg1 peg2 peg3 peg4
  | n <= 0    = []
  | otherwise = hanoi4 (hanoiK n) peg1 peg4 peg2 peg3 ++
                hanoi3 (n - hanoiK n) peg1 peg2 peg3 ++
                hanoi4 (hanoiK n) peg4 peg2 peg3 peg1

-- Helper function for Hanoi
hanoiK :: Integer -> Integer
hanoiK n = n - round (sqrt (fromIntegral (2*n + 1))) + 1
