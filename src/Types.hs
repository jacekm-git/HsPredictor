module Types where

type ThrowsError = Either String
type Match = (Int, String, String, Int, Int)
data Result = Win | Draw | Loss
data Field = Home | Away
