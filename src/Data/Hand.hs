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
