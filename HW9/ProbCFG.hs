module ProbCFG where

---------------------------------------------------------------

import qualified Data.Map as Map

-- A useful helper for debugging with Map. Feel free to ignore 
-- the implementation of this.
printMap :: (Show k, Show v) => Map.Map k v -> IO ()
printMap = putStr . unlines . map show . Map.toList

---------------------------------------------------------------

data Cat = S | NP | VP | N | D | V | PP | P | Adv deriving (Show,Eq,Ord)

data StrucDesc = Leaf Cat String | Binary Cat StrucDesc StrucDesc
                 deriving Show

type ProbCFG = ([(Cat,Double)],
                [((Cat,String),Double)],        -- terminal rules
                [((Cat,(Cat,Cat)),Double)],     -- nonterminal rules
                [Cat])

-- A minor variant of the grammar on page 384 of 
-- Manning and Schutze's "Foundations of Statistical NLP"
pcfg1 :: ProbCFG
pcfg1 = (   [(S,1.0)] ,
            [((P,"with"), 1.0), 
             ((V,"saw"), 1.0), 
             ((NP,"dogs"), 0.1), ((NP,"telescopes"), 0.18), ((NP,"saw"), 0.04), ((NP,"cats"), 0.18), ((NP,"hamsters"), 0.1)] ,
            [((S,(NP,VP)), 1.0),
             ((PP,(P,NP)), 1.0),
             ((VP,(V,NP)), 0.7), ((VP,(VP,PP)), 0.3),
             ((NP,(NP,PP)), 0.4)] ,
            [S,NP,VP,PP,P,V]
        )

-- Like above but reversed probabilities on the rules for expanding VP
pcfg2 :: ProbCFG
pcfg2 = (   [(S,1.0)] ,
            [((P,"with"), 1.0), 
             ((V,"saw"), 1.0), 
             ((NP,"dogs"), 0.1), ((NP,"telescopes"), 0.18), ((NP,"saw"), 0.04), ((NP,"cats"), 0.18), ((NP,"hamsters"), 0.1)] ,
            [((S,(NP,VP)), 1.0),
             ((PP,(P,NP)), 1.0),
             ((VP,(V,NP)), 0.3), ((VP,(VP,PP)), 0.7),
             ((NP,(NP,PP)), 0.4)] ,
            [S,NP,VP,PP,P,V]
        )

--------------------------------------------------
-- Utility functions for getting information from grammars.

probLookup :: (Eq a) => [(a,Double)] -> a -> Double
probLookup []           key = 0.0
probLookup ((x,y):rest) key = if key == x then y else probLookup rest key

allCats :: ProbCFG -> [Cat]
allCats (starting,ending,transitions,cats) = cats

startProb :: ProbCFG -> Cat -> Double
startProb (starting,ending,transitions,cats) = probLookup starting

endProb :: ProbCFG -> Cat -> String -> Double
endProb (starting,ending,transitions,cats) c s = probLookup ending (c,s)

trProb :: ProbCFG -> Cat -> (Cat,Cat) -> Double
trProb (starting,ending,transitions,cats) c (c1,c2) = probLookup transitions (c,(c1,c2))

-------------------------------------------------------------
-- Simple recursive definition of inside probabilities

childCats :: ProbCFG -> Cat -> [(Cat,Cat)]
childCats (starting,ending,[],cats) c = []
childCats (starting,ending,(w:ws),cats) c = 
    if c == fst (fst w) then snd (fst w) : (childCats (starting,ending,ws,cats) c) else (childCats (starting,ending,ws,cats) c)

naiveInside :: ProbCFG -> [String] -> Cat -> Double
naiveInside g [] c = undefined
naiveInside g (w:[]) c = endProb g c w 
naiveInside g w c = 
    sum (map (\child -> sum (map (\i -> trProb g c (fst child,snd child) * naiveInside g (take i w) (fst child) * naiveInside g (drop i w) (snd child)) [1..((length w)-1)])) (childCats g c))

-------------------------------------------------------------

-- A name for the specific Map type that we will use to represent 
-- a table of inside probabilities.
type InsideTable = Map.Map ([String],Cat) Double

fastInside :: ProbCFG -> [String] -> Cat -> Double
fastInside g sent c =
    Map.findWithDefault 0 (sent,c) (buildInsideTable g sent)

buildInsideTable :: ProbCFG -> [String] -> InsideTable
buildInsideTable g sent =
    fillCellsInside g Map.empty (cellsToFill g sent)

cellsToFill :: ProbCFG -> [String] -> [([String],Cat)]
cellsToFill g sent = [(chunk,cat) | chunk <- chunks sent, cat <- allCats g]

sublistsHelper :: [String] -> Int -> [[String]]
sublistsHelper [] i = []
sublistsHelper (w:ws) i = if i <= length (w:ws) then (take i (w:ws)) : sublistsHelper ws i else []

sublists :: [String] -> Int -> [[String]]
sublists [] i = []
sublists [w] i = []
sublists (w:ws) i = concat (map (\i -> take i (w:ws) : sublistsHelper ws i) [i..length (w:ws)])

chunks :: [String] -> [[String]]
chunks [] = []
chunks w = sublists w 1 
--chunks (w:ws) = [take n (w:ws) | n <- [1..length (w:ws)]] ++ chunks ws

fillCellsInside :: ProbCFG -> InsideTable -> [([String],Cat)] -> InsideTable
fillCellsInside g tbl [] = tbl
fillCellsInside g tbl ((chunk,c):rest) = 
    case chunk of 
    [] -> undefined 
    (w:[]) -> let updateTbl = Map.insert (chunk,c) (endProb g c w) tbl in
                  fillCellsInside g updateTbl rest
    w -> let fastProb = \a -> \b -> Map.findWithDefault 0 (a,b) tbl in
             let result = sum (map (\child -> sum (map (\i -> trProb g c (fst child,snd child) * fastProb (take i w) (fst child) * fastProb (drop i w) (snd child)) [1..((length w)-1)])) (childCats g c)) in
                 let updateTbl = Map.insert (chunk,c) result tbl in
                     fillCellsInside g updateTbl rest

-------------------------------------------------------------

-- A name for the specific Map type that we will use to represent 
-- a table of viterbi probabilities and backpointers.
type ViterbiTable = (Map.Map ([String],Cat) Double, Map.Map ([String],Cat) (Cat,Cat,Int))

triples :: [(Cat,Cat)] -> [Int] -> [(Cat,Cat,Int)]
triples x y = concat (map (\x -> (map (\y -> (fst x,snd x,y)) (y))) x)

fillCellsViterbi :: ProbCFG -> ViterbiTable -> [([String],Cat)] -> ViterbiTable
fillCellsViterbi = undefined 

buildViterbiTable :: ProbCFG -> [String] -> ViterbiTable
buildViterbiTable = undefined

extractStrucDesc :: ViterbiTable -> ([String],Cat) -> StrucDesc
extractStrucDesc = undefined

