module Codez where
  
  data CharSpec = C Char | CRange Char Char | CSet [Char]
  type Alphabet = [CharSpec]
  type CodePoint = Either Char Int
  
  parse :: Alphabet -> String -> [CodePoint]
  parse a s = map (parseChar a) s
  
  parseChar :: Alphabet -> Char -> CodePoint
  parseChar a c =
    case maybeIndexOf a c of
      Nothing -> Left c
      Just ix -> Right ix
  
  maybeIndexOf :: Alphabet -> Char -> Maybe Int
  maybeIndexOf a c = Nothing -- implement!
  
  nullAlphabet :: Alphabet
  nullAlphabet = []
      