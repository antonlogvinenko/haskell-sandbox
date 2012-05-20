--function application over 
precedence11 = succ 9 + max 5 4 + 1
precedence12 = (succ 9) + (max 5 4) + 1
precedence21 = succ (9*10)
precedence22 = succ 9 * 10

infixFunction1 = 9 `div` 3
infixFunction2 = div 9 3
infixFunctionPrecedence = 9 `div` 3 + 1

--function application notations: div 9 3; 9 `div` 3
--function application has the highest precedence order

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

--else branch is mandatory
doubleSmallNumber x = if x > 100
                      then x
                           else x * 2
--if statement is an expression; everything is an expression
doubleSmallNumber2 x = (if x > 100 then x else x * 2) + 1

--function without arguments is a definition
cake = "cake is a lie"




--strings are lists
list1 = "cake " ++ "is a lie"

--append, O(length(array1))
list2 = [1, 2] ++ [3, 4]

--prepend O(1)
list3 = 1:[2, 3]
someList = [1, 2, 3, 4]
getItem = someList !! 3
getHead = head someList
getTail = tail someList
getLast = last someList
getInit = init someList
getLength = length someList
getNull1 = null someList
getNull2 = null []
doReverse = reverse someList
doTake = take 2 someList
doDrop = drop 2 someList
getMin = minimum someList
getMax = maximum someList
getSum = sum someList
getProd = product someList
getHasElem = elem 4 someList


getNumRange = [1..20]
getStringRange = ['a'..'z']
getStepRange1 = [20, 19..1]
getStepRange2 = [1, 3..20]
getCycle = take 10 (cycle [1, 2, 3])
getRepeat = take 10 (repeat 5)
getReplicate = replicate 3 5

--list comprehensions
getComp1 = [x*2 | x<-[1..10]]
getComp2 = [x*2 | x<-[1..10], x*2>=12]
boomBang xs = [if x < 10 then "boom!" else "bang!" | x<-xs, odd x]
getComp3 = [x | x <- [1..20], x/=13, x/=15, x/=19]
getComp4 = [x*y | x <- [2, 5, 10], y <- [6, 9, 4]]
getComp5 = [x*y | x <- [2, 5, 10], y <- [6, 9, 7], x*y < 25]
length' xs = sum [1 | _ <- xs]