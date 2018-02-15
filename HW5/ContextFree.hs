module ContextFree where

------------------------------------------------------------------------------------

data Cat = S | NP | VP | V | D | N | PP | P | IV | TV | RC deriving (Show,Eq)

data StrucDesc  = Binary Cat StrucDesc StrucDesc
                | Unary Cat StrucDesc
                | Leaf Cat String
                deriving Show

data GrammarRule    = BinaryStep Cat Cat Cat
                    | UnaryStep Cat Cat
                    | End Cat String
                    deriving Show

type Address = [Int]

grammar1 =  [   -- S rules
                BinaryStep S NP VP,
                -- VP rules
                BinaryStep VP V NP,
                BinaryStep VP V S,
                BinaryStep VP VP PP,
                UnaryStep  VP V,
                -- PP rules
                BinaryStep PP P NP,
                -- D rules
                End D "the",
                End D "a",
                -- N rules
                End N "cat",
                End N "dog",
                -- NP rules
                BinaryStep NP D N,
                End NP "John",
                End NP "Mary",
                -- V rules
                End V "left",
                End V "thinks",
                -- P rules
                End P "with"
            ]

grammar2 = [    BinaryStep S NP IV,
                BinaryStep NP N RC,
                UnaryStep NP N,
                BinaryStep RC NP TV,
                End N "dogs",   End N "cats",
                End IV "chase", End IV "sleep",
                End TV "chase"
            ]

sd1 = Binary S (Leaf NP "John") (Binary VP (Leaf V "left") (Leaf NP "Mary"))

sd2 = Binary S (Leaf NP "John")
               (Binary VP (Unary VP (Leaf V "left")) 
                          (Binary PP (Leaf P "with") (Binary NP (Leaf D "a") (Leaf N "cat")))
               )

sd3 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd1)

sd4 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd2)

sd5 = Binary S (Binary NP (Leaf N "dogs") (Binary RC (Unary NP (Leaf N "dogs")) (Leaf TV "chase"))) (Leaf IV "chase")

------------------------------------------------------------------------------------

pf :: StrucDesc -> [String]
pf (Binary c sd1 sd2) = pf sd1 ++ pf sd2
pf (Unary c sd) = pf sd
pf (Leaf c s) = [s]

leftmostLeaf :: StrucDesc -> String
leftmostLeaf (Leaf c s) = s
leftmostLeaf (Unary c sd) = leftmostLeaf sd
leftmostLeaf (Binary c sd1 sd2) = leftmostLeaf sd1

categoryOf :: StrucDesc -> Cat
categoryOf (Leaf c s) = c
categoryOf (Unary c sd) = c
categoryOf (Binary c sd1 sd2) = c

wellFormed :: [GrammarRule] -> StrucDesc -> Bool
wellFormed g (Leaf c s) = elem c (enders g s)
wellFormed g (Unary c sd) = elem c (predecessorsUnary g (categoryOf sd)) && wellFormed g sd
wellFormed g (Binary c sd1 sd2) = elem c (predecessorsBinary g (categoryOf sd1, categoryOf sd2)) 
                                            && wellFormed g sd1 && wellFormed g sd2

depth :: StrucDesc -> Int
depth (Leaf c s) = 1
depth (Unary c sd) = 1 + depth sd
depth (Binary c sd1 sd2) = if (depth sd1 > depth sd2) then (1 + depth sd1) else (1 + depth sd2)

enders :: [GrammarRule] -> String -> [Cat]
enders [] x = []
enders (r:rs) x =
    case r of
    End c s -> if s == x then c : (enders rs x) else enders rs x
    UnaryStep c ch -> enders rs x
    BinaryStep c ch1 ch2 -> enders rs x

predecessorsUnary :: [GrammarRule] -> Cat -> [Cat]
predecessorsUnary [] x = []
predecessorsUnary (r:rs) x =
    case r of
    End c s -> predecessorsUnary rs x
    UnaryStep c ch -> if ch == x then (c : (predecessorsUnary rs x)) else (predecessorsUnary rs x)
    BinaryStep c ch1 ch2 -> predecessorsUnary rs x

predecessorsBinary :: [GrammarRule] -> (Cat,Cat) -> [Cat]
predecessorsBinary [] x = []
predecessorsBinary (r:rs) x =
    case r of
    End c s -> predecessorsBinary rs x
    UnaryStep c ch -> predecessorsBinary rs x
    BinaryStep c ch1 ch2 -> if (ch1,ch2) == x then (c : (predecessorsBinary rs x)) else (predecessorsBinary rs x)

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

brackets :: StrucDesc -> String
brackets (Binary c sd1 sd2) = "[" ++ brackets sd1 ++ " " ++ brackets sd2 ++ "]"
brackets (Unary c sd) = "[" ++ brackets sd ++ "]"
brackets (Leaf c s) = s

labeledBrackets :: StrucDesc -> String
labeledBrackets (Binary c sd1 sd2) = "[" ++ show c ++ " " ++ labeledBrackets sd1 ++ " " ++ labeledBrackets sd2 ++ "]"
labeledBrackets (Unary c sd) = "[" ++ show c ++ " " ++ labeledBrackets sd ++ "]"
labeledBrackets (Leaf c s) = s 

numNPs :: StrucDesc -> Int
numNPs (Binary c sd1 sd2) = if c == NP then 1 + numNPs sd1 + numNPs sd2 else numNPs sd1 + numNPs sd2
numNPs (Unary c sd) = if c == NP then 1 + numNPs sd else numNPs sd
numNPs (Leaf c s) =  if c == NP then 1 else 0

numViolations :: [GrammarRule] -> StrucDesc -> Int
numViolations g (Binary c sd1 sd2) = if (wellFormed g (Binary c sd1 sd2)) == True then numViolations g sd1 + numViolations g sd2 else 1 + numViolations g sd1 + numViolations g sd2
numViolations g (Unary c sd) = if (wellFormed g (Unary c sd)) == True then numViolations g sd else 1 + numViolations g sd
numViolations g (Leaf c s) = if (wellFormed g (Leaf c s)) == True then 0 else 1

sdMap :: (String -> String) -> StrucDesc -> StrucDesc
sdMap f (Binary c sd1 sd2) = (Binary c (sdMap f sd1) (sdMap f sd2))
sdMap f (Unary c sd) = (Unary c (sdMap f sd))
sdMap f (Leaf c s) = (Leaf c (f s))

longestPath :: StrucDesc -> [Cat]
longestPath (Binary c sd1 sd2) = if (length (longestPath sd1)) >= (length (longestPath sd2)) then c : (longestPath sd1) else c : (longestPath sd2)
longestPath (Unary c sd) = c : longestPath sd
longestPath (Leaf c s) = [c]

allPaths :: StrucDesc -> [[Cat]]
allPaths (Binary c sd1 sd2) = map (\a -> c : a) (allPaths sd1) ++ map (\a -> c : a) (allPaths sd2)
allPaths (Unary c sd) = map (\a -> c : a) (allPaths sd)
allPaths (Leaf c s) = [[c]]

addressesOfNPs :: StrucDesc -> [Address]
addressesOfNPs (Binary c sd1 sd2) = if c == NP then [[]] ++ map (\a -> a ++ [0]) (addressesOfNPs sd1) ++ map (\a -> a ++ [1]) (addressesOfNPs sd2)
                                    else map (\a -> a ++ [0]) (addressesOfNPs sd1) ++ map (\a -> a ++ [1]) (addressesOfNPs sd2)
addressesOfNPs (Unary c sd) = if c == NP then [[]] ++ map (\a -> a ++ [0]) (addressesOfNPs sd) else map (\a -> a ++ [0]) (addressesOfNPs sd)
addressesOfNPs (Leaf c s) = if c == NP then [[]] else []

ccommand :: Address -> Address -> Bool
ccommand w [] = False
ccommand (w:[]) (v:vs) = if w == v then False else True
ccommand (w:ws) (v:vs) = if w == v then True && ccommand ws vs else False

replace :: StrucDesc -> Address -> StrucDesc -> StrucDesc
replace (Binary c sd1 sd2) addr sd' = case addr of 
                                      (w:[]) -> if w == 0 then (Binary c sd' sd2) else (Binary c sd1 sd')
                                      (w:ws) -> if w == 0 then (Binary c (replace sd1 ws sd') sd2) else (Binary c sd1 (replace sd2 ws sd'))
replace (Unary c sd) addr sd' = case addr of 
                                (w:[]) -> if w == 0 then (Unary c sd') else (Unary c sd)
                                (w:ws) -> if w == 0 then (Unary c (replace sd ws sd')) else (Unary c sd)
replace (Leaf c s) addr sd' = case addr of
                              [] -> sd'
                              (w:ws) -> (Leaf c s)

getSdAtAddress :: StrucDesc -> Address -> StrucDesc
getSdAtAddress (Binary c sd1 sd2) addr = case addr of 
                                         (w:[]) -> if w == 0 then sd1 else sd2
                                         (w:ws) -> if w == 0 then getSdAtAddress sd1 ws else getSdAtAddress sd2 ws
getSdAtAddress (Unary c sd) addr = case addr of
                                          (w:[]) -> sd
                                          (w:ws) -> getSdAtAddress sd ws 
getSdAtAddress (Leaf c s) addr = (Leaf c s)
                                 
move :: Address -> StrucDesc -> StrucDesc
move addr sd = Binary S (getSdAtAddress sd addr) (replace sd addr (Leaf (categoryOf (getSdAtAddress sd addr)) "t"))

