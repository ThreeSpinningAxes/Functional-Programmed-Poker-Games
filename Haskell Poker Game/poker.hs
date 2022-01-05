--Created by: Devrajsinh Chudasama, ID#: 500975539
--            Mayank Kainth, ID#: 500950561



module Poker where

import Data.List 
import Data.Maybe

getRank num = do
  let temp = rem num 13
  if temp > 1 && temp <= 12 then
   temp
  else
   if temp == 0 then 13
    else 14
 
getSuit card = do
  if card  >= 0 && card < 14 then
   "C"
  else
   if card >= 14 && card <= 26 then
   "D"
   else
    if card >= 27 && card <= 39 then
     "H"
    else
     "S"
         
cardFormat list = map (\x -> (getRank x, getSuit x)) list
 
sortDecending list = reverse (sort list)

countElemInList x list = length (filter ((==x).fst) list)

getRankCount list = do
 let rankCountsList1 = [countElemInList 14 list] 
 let rankCountsList2 = rankCountsList1 ++ [countElemInList 2 list]
 let rankCountsList3 = rankCountsList2 ++ [countElemInList 3 list]
 let rankCountsList4 = rankCountsList3 ++ [countElemInList 4 list]
 let rankCountsList5 = rankCountsList4 ++ [countElemInList 5 list]
 let rankCountsList6 = rankCountsList5 ++ [countElemInList 6 list]
 let rankCountsList7 = rankCountsList6 ++ [countElemInList 7 list]
 let rankCountsList8 = rankCountsList7 ++ [countElemInList 8 list]
 let rankCountsList9 = rankCountsList8 ++ [countElemInList 9 list]
 let rankCountsList10 = rankCountsList9 ++ [countElemInList 10 list]
 let rankCountsList11 = rankCountsList10 ++ [countElemInList 11 list]
 let rankCountsList12 = rankCountsList11 ++ [countElemInList 12 list]
 let rankCountsList13 = rankCountsList12 ++ [countElemInList 13 list]
 rankCountsList13
 
getSuitCount cardList = do
  let cCount = length (filter ((=="C").snd) cardList)
  let dCount = length (filter ((=="D").snd) cardList)
  let hCount = length (filter ((=="H").snd) cardList)
  let sCount = length (filter ((=="S").snd) cardList)
  [cCount, dCount, hCount, sCount] 
  
getSuitOfFlush suitCounterList = do
 let index = elemIndex (maximum suitCounterList) suitCounterList
 if maximum suitCounterList >= 5 then
  case index of
   Just 0 -> "C"
   Just 1 -> "D"
   Just 2 -> "H"
   Just 3 -> "S"
  else
   "False"
 
uniqRank list = nubBy uniqRankCmp list
  where
   uniqRankCmp (x, y) (x', y') = x == x'
 

checkRoyalFlush list = do
 let suitCounterList = getSuitCount list
 let commonSuit = getSuitOfFlush suitCounterList
 if (14, commonSuit) `elem` list  && (13, commonSuit) `elem` list && (12, commonSuit) `elem` list && (11, commonSuit) `elem` list && (10, commonSuit) `elem` list then
  [(14, commonSuit), (13, commonSuit), (12, commonSuit), (11, commonSuit), (10, commonSuit)]
 else
  []
  
  

checkFlush list = do
 let suitCounterList = getSuitCount list
 let suit = getSuitOfFlush suitCounterList
 if suit /= "False" then 
  take 5 (filter ((==suit).snd) list)
 else
  []
 
check4OfAKind list = do
 let rankCounterList = getRankCount list
 if (4 `elem` rankCounterList) == True then do
  let rank = fromJust (elemIndex 4 rankCounterList)
  if rank == 0 then 
   filter ((==14).fst) list
  else
   filter ((==(rank+1)).fst) list
 else
  []

check3OfAKind list = do
 let rankCounterList = getRankCount list
 if (3 `elem` rankCounterList) == True then do
  let listOfThrees = findIndices (\x -> x == 3) rankCounterList

  if (head listOfThrees) == 0 then do
   let firstThree = filter ((==14).fst) list
   firstThree
  else do
   let index = last listOfThrees
   let firstThree = filter ((==(index+1)).fst) list
   firstThree
 else
  []
  
checkFullHouse list = do
 let threePair = check3OfAKind list
 let newList = list \\ threePair
 
 let rankCounterList = getRankCount newList
 let listOfPairs = findIndices (>=2) rankCounterList
 if length listOfPairs == 0 || length threePair == 0 then
  []
 else do
  if (head listOfPairs) == 0 then do
   let firstPair = take 2 (filter ((==14).fst) newList)
   sortDecending (threePair ++ firstPair)
  else do
   let index = last listOfPairs
   let firstPair = take 2 (filter ((==(index+1)).fst) newList)
   sortDecending (threePair ++ firstPair)
 

checkStraightHelper list = do
 
 let rankList = uniqRank list
 
 let sequence1 = take 5 rankList
 let sequence2 = take 5 (tail rankList)
 let sequence3 = take 5 (tail (tail rankList))
 
 let seq1RankOnly = (map (\(x, y) -> x) sequence1)
 let seq2RankOnly = (map (\(x, y) -> x) sequence2)
 let seq3RankOnly = (map (\(x, y) -> x) sequence3)
 
 if length sequence1 == 5 then
  if seq1RankOnly == reverse [head ( drop (length sequence1 - 1) seq1RankOnly)..( head seq1RankOnly )] then
   sequence1
  else
   if length sequence2 == 5 then
    if seq2RankOnly == reverse [head ( drop (length sequence2 - 1) seq2RankOnly)..( head seq2RankOnly )] then
     sequence2
    else
     if length sequence3 == 5 then
      if seq3RankOnly == reverse [head ( drop (length sequence3 - 1) seq3RankOnly)..( head seq3RankOnly )] then
       sequence3
      else
       []
     else
      []
   else
    []
 else
  []


checkStraight list = do
 let x = checkStraightHelper list
 if x /= [] then
  x
 else
  checkStraightHelper (sortDecending (map ( \(x, y) -> if (x==14) then (x-13, y) else (x,y) ) list))
  
checkStraightFlushHelper list = do
 let suitCounterList = getSuitCount list
 let commonSuit = getSuitOfFlush suitCounterList
 let rankList = uniqRank (filter ((==commonSuit).snd) list)
 
 let sequence1 = take 5 rankList
 let sequence2 = take 5 (tail rankList)
 let sequence3 = take 5 (tail (tail rankList))
 
 let seq1RankOnly = (map (\(x, y) -> x) sequence1)
 let seq2RankOnly = (map (\(x, y) -> x) sequence2)
 let seq3RankOnly = (map (\(x, y) -> x) sequence3)
 
 if length sequence1 == 5 then
  if seq1RankOnly == reverse [head ( drop (length sequence1 - 1) seq1RankOnly)..( head seq1RankOnly )] && all (\(x, y) -> (x, y) == (x, commonSuit)) sequence1 then
   sequence1
  else
   if length sequence2 == 5 then
    if seq2RankOnly == reverse [head ( drop (length sequence2 - 1) seq2RankOnly)..( head seq2RankOnly )] && all (\(x, y) -> (x, y) == (x, commonSuit)) sequence2 then
     sequence2
    else
     if length sequence3 == 5 then
      if seq3RankOnly == reverse [head ( drop (length sequence3 - 1) seq3RankOnly)..( head seq3RankOnly )] && all (\(x, y) -> (x, y) == (x, commonSuit)) sequence3 then
       sequence3
      else
       []
     else
      []
   else
    []
 else
  []
 
checkPair list = do
 let rankCounterList = getRankCount list

 if (2 `elem` rankCounterList) == True then do
  let rank = 12 - fromJust (elemIndex 2 (reverse rankCounterList))
  if rank == 0 then
   filter ((==14).fst) list
  else 
   filter ((==rank+1).fst) list
 else
  []

checkTwoPair list = do
 let rankCounterList = getRankCount list
 let twoCount = length (findIndices (==2) rankCounterList)

 if (2 `elem` rankCounterList) == True && twoCount >= 2 then do
  let indices = reverse (findIndices (==2) rankCounterList)
  if length indices == 0 then
   []
  else
   if last indices == 0 then 
    (filter ((==14).fst) list) ++ (filter ((==indices!!0 + 1).fst) list)
   else do
    (filter ((==indices!!0 + 1).fst) list) ++ (filter ((==indices!!1 + 1).fst) list)
 else 
  []
 
checkStraightFlush list = do
 let x = checkStraightFlushHelper list
 if x /= [] then
  x
 else
  checkStraightFlushHelper (sortDecending (map ( \(x, y) -> if (x==14) then (x-13, y) else (x,y) ) list))
  
getHighestCard list =  [head (sortDecending list)]
  
toString list = do
  --let string = sort list
  --let string2 = (map (\(x, y) -> if (x==14) then (x-13, y) else (x,y)) list)
  --map (\(x, y) -> (show(x) ++ y)) string2
 let string = (map (\(x, y) -> if (x==14) then (x-13, y) else (x,y)) (sort list))
 map (\(x, y) -> (show(x) ++ y)) string
  
  
tieBreakerStraightFlush hand1 hand2 = do
 let h1 = checkStraightFlush hand1
 let h2 = checkStraightFlush hand2
 
 let highestRank1 = fst (head h1)
 let highestRank2 = fst (head h2)
 
 if (highestRank1 > highestRank2) then
  h1
 else
  h2

tieBreakerFourOfAKind hand1 hand2 = do
 let h1 = check4OfAKind hand1
 let h2 = check4OfAKind hand2
 
 let highestRank1 = fst (head h1)
 let highestRank2 = fst (head h2)
 
 if (highestRank1 > highestRank2) then
  h1
 else
  h2

tieBreakerHighCard hand1 hand2 = do
 let x = (map (\(x, y) -> x) hand1) \\ (map (\(x, y) -> x) hand2)
 let y = (map (\(x, y) -> x) hand2) \\ (map (\(x, y) -> x) hand1)
 
 if length x == 0 then
  take 1 hand1
 else do
  let highestRank1 = head x
  let highestRank2 = head y
  if highestRank1 > highestRank2 then
   take 1 hand1
  else
   take 1 hand2
 
tieBreakerFlush hand1 hand2 = do
 
 let h1 = checkFlush hand1
 let h2 = checkFlush hand2
 
 let x = h1 \\ h2
 let y = h2 \\ h1
 
 if length x == 0 then
  h1
 else do
  let highestRank1 = fst (head x)
  let highestRank2 = fst (head y)
  if highestRank1 > highestRank2 then
   h1
  else
   h2
 
tieBreakerStraight hand1 hand2 = do
 let h1 = checkStraight hand1
 let h2 = checkStraight hand2
 
 let highestRank1 = fst (head h1)
 let highestRank2 = fst (head h2)
 
 if highestRank1 > highestRank2 then
  h1
 else
  h2

tieBreaker3OfAKind hand1 hand2 = do
 let h1 = check3OfAKind hand1
 let h2 = check3OfAKind hand2
 
 let highestRank1 = fst (head h1)
 let highestRank2 = fst (head h2)
 
 if highestRank1 > highestRank2 then
  h1
 else if highestRank2 > highestRank1 then
  h2
 else do 
  let remainingCards1 = take 2 (sortDecending (hand1 \\ h1))
  let remainingCards2 = take 2 (sortDecending (hand2 \\ h2))
  
  if length remainingCards1 == 0 then do
   h1
  else do
   let x = remainingCards1 \\ remainingCards2
   let y = remainingCards2 \\ remainingCards1
  
   if length x /= 0 then do
    let highestCard1 = fst (head x)
    let highestCard2 = fst (head y)
    if highestCard1 > highestCard2 then
     h1
    else
     h2
   else
    h1
    
tieBreakerFullHouse hand1 hand2 = do
 let h1 = checkFullHouse hand1
 let h2 = checkFullHouse hand2
 
 let threeKind1 = check3OfAKind h1
 let threeKind2 = check3OfAKind h2
 
 let pair1 = h1 \\ threeKind1
 let pair2 = h2 \\ threeKind2
 
 if (fst (head threeKind1)) > (fst (head threeKind2)) then
  h1
 else if (fst (head threeKind1)) < (fst (head threeKind2)) then
  h2
 else 
  if (fst (head pair1)) > (fst (head pair2)) then
   h1
  else
   h2

tieBreakerPair hand1 hand2 = do
 let pair1 = checkPair hand1
 let pair2 = checkPair hand2

 let rank1 = fst (head pair1)
 let rank2 = fst (head pair2)

 if rank1 > rank2 then 
   pair1
  else 
   if rank2 > rank1 then 
     pair2
   else do
    let newhand1 = hand1 \\ pair1
    let newhand2 = hand2 \\ pair2

    let x = newhand1 \\ newhand2
    let y = newhand2 \\ newhand1

    if length x == 0 then
     pair1
    else do
     let highest1 = fst (head x)
     let highest2 = fst (head y)
     if highest1 > highest2 then
      pair1
     else
      pair2


 
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

tieBreakerTwoPair hand1 hand2 = do
 let pair1 = checkTwoPair hand1
 let pair2 = checkTwoPair hand2

 let rank1 = fst (head pair1)
 let rank2 = fst (head pair2)

 if rank1 > rank2 then
   pair1
  else do
   if rank2 > rank1 then
    pair2
    
    else do
     let highest1 = fst (head (slice 2 3 pair1)) 
     let highest2 = fst (head (slice 2 3 pair2))

     if highest1 > highest2 then 
      pair1
      else do
       if highest2 > highest1 then
        pair2
        else do
         let newhand1 = hand1 \\ pair1
         let newhand2 = hand2 \\ pair2

         let x = newhand1 \\ newhand2
         let y = newhand2 \\ newhand1

         if length x == 0 then
          pair1
         else do
          let newhighest1 = fst (head x)
          let newhighest2 = fst (head y) 
          if newhighest1 > newhighest2 then
           pair1
          else
           pair2 
  
determineHand hand = do
 if checkRoyalFlush hand /= [] then
  (hand, 10)
 else
  if checkStraightFlush hand /= [] then
   (hand, 9)
  else
   if check4OfAKind hand /= [] then
    (hand, 8)
   else
    if checkFullHouse hand /= [] then
     (hand, 7)
    else
     if checkFlush hand /= [] then
      (hand, 6)
     else
      if checkStraight hand /= [] then
       (hand, 5)
      else
       if check3OfAKind hand /= [] then
        (hand, 4)
       else
        if checkTwoPair hand /= [] then
         (hand, 3)
        else
         if checkPair hand /= [] then
          (hand, 2)
         else
         (hand, 1)




tieBreaker hand1 hand2 = do
 let x = snd hand1
 case x of
  10 -> checkRoyalFlush (fst hand1)
  9 -> tieBreakerStraightFlush (fst hand1) (fst hand2)
  8 -> tieBreakerFourOfAKind (fst hand1) (fst hand2)
  7 -> tieBreakerFullHouse (fst hand1) (fst hand2)
  6 -> tieBreakerFlush (fst hand1) (fst hand2)
  5 -> tieBreakerStraight (fst hand1) (fst hand2)
  4 -> tieBreaker3OfAKind (fst hand1) (fst hand2)
  3 -> tieBreakerTwoPair (fst hand1) (fst hand2)
  2 -> tieBreakerPair(fst hand1) (fst hand2)
  1 -> tieBreakerHighCard (fst hand1) (fst hand2)
  
determineWinner hand1 hand2 = do
 let player1Hand = determineHand hand1
 let player2Hand = determineHand hand2
 
 if (snd player1Hand) > (snd player2Hand) then do
  let x = snd player1Hand
  case x of
   10 -> checkRoyalFlush hand1
   9 -> checkStraightFlush hand1
   8 -> check4OfAKind hand1
   7 -> checkFullHouse hand1
   6 -> checkFlush hand1
   5 -> checkStraight hand1
   4 -> check3OfAKind hand1
   3 -> checkTwoPair hand1
   2 -> checkPair hand1
   1 -> getHighestCard hand1
 else if (snd player1Hand) < (snd player2Hand) then do
  let x = snd player2Hand
  case x of
   10 -> checkRoyalFlush hand2
   9 -> checkStraightFlush hand2
   8 -> check4OfAKind hand2
   7 -> checkFullHouse hand2
   6 -> checkFlush hand2
   5 -> checkStraight hand2
   4 -> check3OfAKind hand2
   3 -> checkTwoPair hand2
   2 -> checkPair hand2
   1 -> getHighestCard hand2
 else
  if (snd player1Hand) == (snd player2Hand) then
   tieBreaker player1Hand player2Hand
  else
   [(123, "error")] 


deal (cardList) = do
  let pool = [ cardList!!4, cardList!!5, cardList!!6, cardList!!7, cardList!!8]
  let player1Hand = [ cardList!!0, cardList!!2 ]
  let player2Hand = [ cardList!!1, cardList!!3 ]
  
  let player1FullHand = sortDecending ( cardFormat (player1Hand ++ pool) )
  let player2FullHand = sortDecending ( cardFormat (player2Hand ++ pool) )
  
  toString (determineWinner player1FullHand player2FullHand)
  
