{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

data Number = Integer | Float deriving (Show, Ord, Eq)
data NumberTwo = Infinity | NegativeInfinity | Fuck Number deriving (Show)

main = do
  print $ Fuck 3.57
  print $ Fuck 4