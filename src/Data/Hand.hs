module Data.Hand
    ( Hand
    , toHand, fromHand
    ) where

import qualified Data.Card as Card
import qualified Data.List as List
import Data.Semigroup ((<>))
newtype Hand = Hand { fromHand :: [Card.Card] } deriving (Show, Eq, Ord)

toHand :: [Card.Card] -> Maybe Hand
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

flushHint :: Hand -> Bool
flushHint (Hand h) =
  all ((Card.cardSuit (head h) ==) . Card.cardSuit) h

nOfKindHint :: Int -> Hand -> Maybe [[Card.Card]]
nOfKindHint n (Hand h) = if cards /= [] then Just cards else Nothing
  where
    cards :: [[Card.Card]]
    cards = filter ((==n).length) $
              List.groupBy (\x y -> Card.cardNumber x == Card.cardNumber y) h

straightHint :: Hand -> Bool
straightHint (Hand l) =
  (judgeStraight . extract Card.cardStrength $ l) || (judgeStraight . List.sort . extract Card.cardNumber $ l)
  where
    isStraight :: [Int] -> Bool
    isStraight xs@(x:_) = xs == [x .. x + 4]
    isStraight _ = False

    judgeStraight :: [(Int, Card.Card)] -> Bool
    judgeStraight l = isStraight $ map fst l

    extract :: (b -> a) -> [b] -> [(a, b)]
    extract f = map (\c -> (f c, c))
