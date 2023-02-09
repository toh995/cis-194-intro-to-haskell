{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

type AttackerDieValue = DieValue
type DefenderDieValue = DieValue

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

-- Exercise 2
type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield@(Battlefield {attackers, defenders}) = do
  let numAtkDice = min 3 (attackers - 1)
  let numDefDice = min 2 defenders

  atkDieVals <- replicateM numAtkDice die
  defDieVals <- replicateM numDefDice die

  return battlefield {
      attackers = attackers - numAtkLosses atkDieVals defDieVals
    , defenders = defenders - numDefLosses atkDieVals defDieVals
  }

numAtkLosses :: [AttackerDieValue] -> [DefenderDieValue] -> Int
numAtkLosses atkDieVals defDieVals =
  let sortedAtks = reverse . sort $ atkDieVals
      sortedDefs = reverse . sort $ defDieVals
      zipped = zip sortedAtks sortedDefs
   in length . filter (uncurry (<=)) $ zipped

numDefLosses :: [AttackerDieValue] -> [DefenderDieValue] -> Int
numDefLosses atkDieVals defDieVals =
  let sortedAtks = reverse . sort $ atkDieVals
      sortedDefs = reverse . sort $ defDieVals
      zipped = zip sortedAtks sortedDefs
   in length . filter (uncurry (>)) $ zipped

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf =
  battle bf >>= \bf' -> if
    | defenders bf' <= 0 -> return bf'
    | attackers bf' < 2  -> return bf'
    | otherwise          -> invade bf'

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  let denom = 1000
  bfs <- replicateM 1000 (invade bf)
  let num = fromIntegral
          . length
          . filter (\(Battlefield {defenders}) -> defenders <= 0)
          $ bfs
  return $ num / denom

-- Extra function to test everything
main :: IO ()
main = do
  battlefield <- evalRandIO $ successProb (Battlefield {attackers=3, defenders=3})
  print battlefield
