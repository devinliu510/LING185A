-- Devin Liu
-- Section 1A
module Assignment02 where

data Numb = Z | S Numb deriving Show

add :: Numb -> Numb -> Numb
add m n = case n of {Z -> m; S n' -> S (add m n')}

one, two, three, four, five, six :: Numb
one = S Z
two = S one
three = S two
four = S three
five = S four
six = S five

data NumbList = EmptyNL | NonEmptyNL Numb NumbList deriving Show

list0, list1, list2 :: NumbList
list0 = NonEmptyNL one (NonEmptyNL two (NonEmptyNL three EmptyNL))
list1 = NonEmptyNL four (NonEmptyNL Z (NonEmptyNL two EmptyNL))
list2 = NonEmptyNL six (NonEmptyNL one (NonEmptyNL three (NonEmptyNL four EmptyNL)))

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

sumUpTo :: Numb -> Numb
sumUpTo n = case n of {Z -> n; S n' -> (add n (sumUpTo n'))}

equal :: Numb -> (Numb -> Bool)
equal = \n -> (\m -> case n of
                     Z -> case m of {Z -> True; S n' -> False}
                     S n' -> case m of {Z -> False; S m' -> (equal n' m')}
               )

difference :: Numb -> (Numb -> Numb)
difference = \n -> (\m -> case n of
                          Z -> case m of {Z -> Z; S n' -> S n'}
                          S n' -> case m of {Z -> S n'; S m' -> (difference n' m')}
                    )

total :: NumbList -> Numb 
total = \n -> case n of
              {NonEmptyNL n' EmptyNL -> n'; NonEmptyNL n' m -> (add n' (total m))}

incrementAll :: Numb -> (NumbList -> NumbList) 
incrementAll = \n -> (\m -> case n of
                            Z -> m
                            k -> case m of {NonEmptyNL n' EmptyNL -> NonEmptyNL (add n' k) EmptyNL; NonEmptyNL n' m' -> NonEmptyNL (add n' k) (incrementAll k m')}
                      )

addToEnd :: Numb -> (NumbList -> NumbList)
addToEnd = \n -> (\nl -> case nl of 
                         NonEmptyNL n' EmptyNL -> NonEmptyNL n' (NonEmptyNL n EmptyNL)
                         NonEmptyNL n' m' -> NonEmptyNL n' (addToEnd n m')
                  )

lastElement :: NumbList -> Numb
lastElement = \nl -> case nl of
                     {EmptyNL -> Z; NonEmptyNL nl' EmptyNL -> nl'; NonEmptyNL nl' n' -> lastElement n'}

contains :: (Numb -> Bool) -> (NumbList -> Bool)
contains = \f -> (\nl -> case nl of
                         EmptyNL -> False
                         NonEmptyNL nl' EmptyNL -> if (f nl') then True else False
                         NonEmptyNL nl' m' -> if (f nl') then True else contains f m'
                  )

remove :: (Numb -> Bool) -> (NumbList -> NumbList)
remove = \f -> (\nl -> case nl of 
                       EmptyNL -> EmptyNL
                       NonEmptyNL nl' EmptyNL -> if (f nl') then EmptyNL else NonEmptyNL nl' EmptyNL
                       NonEmptyNL nl' m' -> if (f nl') then (remove f m') else NonEmptyNL nl' (remove f m')
                )

append :: NumbList -> (NumbList -> NumbList)
append = \nl -> (\nl2 -> case nl2 of
                        EmptyNL -> nl
                        _ -> (case nl of 
                                   EmptyNL -> nl2
                                   NonEmptyNL n EmptyNL -> NonEmptyNL n nl2
                                   NonEmptyNL n nl' -> NonEmptyNL n (append nl' nl2)
                              )
                 )

prefix :: Numb -> (NumbList -> NumbList)
prefix = \n -> (\nl -> case n of
                       Z -> EmptyNL
                       S n' -> case nl of
                               EmptyNL -> EmptyNL
                               NonEmptyNL m EmptyNL -> NonEmptyNL m EmptyNL
                               NonEmptyNL m nl' -> NonEmptyNL m (prefix n' nl')
                )