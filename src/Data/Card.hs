module Data.Card
    ( Suit(..)
    , Card
    , cardSuit
    , cardNumber
    , decision
    ) where

import           Data.List (sort)

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Read, Eq, Ord, Enum)

data Card = Card Int Suit
  deriving (Eq, Ord)

showCardNumber :: Int -> String
showCardNumber 14 = "A"
showCardNumber 13 = "K"
showCardNumber 12 = "Q"
showCardNumber 11 = "J"
showCardNumber 10 = "10"
showCardNumber x  = show x

instance Show Card where
  show (Card i Hearts)   = "heart "   ++ showCardNumber i
  show (Card i Diamonds) = "diamond " ++ showCardNumber i
  show (Card i Clubs)    = "club "    ++ showCardNumber i
  show (Card i Spades)   = "spade "   ++ showCardNumber i

cardSuit :: Card -> Suit
cardSuit (Card _ s) = s

cardNumber :: Card -> Int
cardNumber (Card n _) = n

decision :: [Card] -> Maybe [Card]
decision l =
    if length l == 5
    then Just $ sort l
    else Nothing
