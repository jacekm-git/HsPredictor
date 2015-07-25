module Types where

type ThrowsError = Either String
type Match = (Int, String, String, Int, Int, Double, Double, Double)
data Result = Win | Draw | Loss | Upcoming
data Field = Home | Away
