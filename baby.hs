--function application over 
precedence11 = succ 9 + max 5 4 + 1
precedence12 = (succ 9) + (max 5 4) + 1
precedence21 = succ (9*10)
precedence22 = succ 9 * 10

infixFunction1 = 9 `div` 3
infixFunction2 = div 9 3
infixFunctionPrecedence = 9 `div` 3 + 1

cake = 81 `div` 9 `div` 3 + 1

--function application notations: div 9 3; 9 `div` 3
--function application has the highest precedence order

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x < 100
                      then x
                           else x * 2