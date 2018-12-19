module Data.Hand
    ( Hand
    , toHand, fromHand
    ) where

import Data.Card ( Card )
import qualified Data.List as List

newtype Hand = Hand { fromHand :: [Card] } deriving (Show, Eq, Ord)

toHand :: [Card] -> Maybe Hand
toHand l =
  if length l == 5
    then Just $ Hand (List.sort l)
    else Nothing

pokerHand :: Hand -> PokerHand
pokerHand = undefined

data PokerHand
    = HighCards
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | Straight
    | Flush
    | FullHouse
    | FourOfAKind
    | StraightFlush
    deriving (Read, Eq, Ord, Enum)

instance Show PokerHand where
  show = showPokerHand

showPokerHand :: PokerHand -> String
showPokerHand HighCards = "high card"
showPokerHand OnePair = "one pair"
showPokerHand TwoPair = "two pair"
showPokerHand ThreeOfAKind = "three of a kind"
showPokerHand Straight = "straight"
showPokerHand Flush = "flush"
showPokerHand FullHouse = "full house"
showPokerHand FourOfAKind = "four of a kind"
showPokerHand StraightFlush = "straight flush"

onePair :: Hand -> Maybe PokerHand
onePair = undefined

twoPair :: Hand -> Maybe PokerHand
twoPair = undefined

threeOfKind :: Hand -> Maybe PokerHand
threeOfKind = undefined

straight :: Hand -> Maybe PokerHand
straight = undefined

flush :: Hand -> Maybe PokerHand
flush = undefined

fullHouse :: Hand -> Maybe PokerHand
fullHouse = undefined

fourOfAKind :: Hand -> Maybe PokerHand
fourOfAKind = undefined

straightFlush :: hand -> Maybe PokerHand
straightFlush = undefined
