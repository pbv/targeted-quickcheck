{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           Test.Hspec 
import           Test.QuickCheck
import           Test.QuickCheck.Targeted
import           Data.List
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Tree
import           Data.Graph (Vertex)
import qualified Data.Graph as Graph


------------------------------------------------------------------------
-- The N queens puzzle
-- See e.g. https://en.wikipedia.org/wiki/Eight_queens_puzzle
------------------------------------------------------------------------

type Queens = [(Int,Int)]

-- generate `n' queens in a 8x8 chess board,
-- ensuring generated coordinates are all distinct
genQueens :: Int -> Gen Queens
genQueens n
  = vectorOf n (genCoord 8) `suchThat` (\xs -> xs == nub xs)

genCoord :: Int -> Gen (Int,Int)
genCoord size = do
  x <- choose (1,size)
  y <- choose (1,size)
  return (x,y)

-- check if two queens attack each other
checkAttack :: (Int,Int) -> (Int,Int) -> Bool
checkAttack (x,y) (x',y')
  = x==x' || y == y' || x+y == x'+y' || x-y == x'-y'

-- count the total number of attacks in the board
attacks :: Queens -> Int
attacks coords
  = sum [ sum (map (fromEnum . checkAttack q) qs) | q:qs <- tails coords ]

-- solve the N queens problem approximately using Hill Climbing
solveQueensHC :: Int -> Int -> Gen Queens
solveQueensHC n iters
  = genQueens n >>=
    boundedClimbing (Minimize attacks) nextQueens iters

-- solve the N queens problem using Simulated Annealing
solveQueensSA :: Int -> Int -> Gen Queens
solveQueensSA n iters
  = genQueens n >>=
    simulatedAnnealing (Minimize attacks) nextQueens schedLinear iters


-- Neightbouring function for the queen's positions;
-- try to change one queen at a time
nextQueens :: Queens -> Gen Queens
nextQueens coords  = do
  coord  <- elements coords
  -- ensure that we dont move the queen over another one
  coord' <- genCoord 8 `suchThat` (\c -> all (/=c) coords)
  return (coord' : delete coord coords)

-- Generate solution attempts and measure the number of attacks
-- as a proxy for search quality

-- baseline: just a random board
prop_measure_attacks_baseline :: Property
prop_measure_attacks_baseline 
  = forAll (genQueens 8) $ \qs -> collect (attacks qs) True

-- solve using bounded Hill Climbing
prop_measure_attacks_hc :: Int -> Property
prop_measure_attacks_hc iters
  = forAll (solveQueensHC 8 iters) $ \qs -> collect (attacks qs) True

-- solve using Simulated Annealing 
prop_measure_attacks_sa :: Int -> Property
prop_measure_attacks_sa iters
  = forAll (solveQueensSA 8 iters) $ \qs -> collect (attacks qs) True

-----------------------------------------------------------------------
-- Coins and change
-----------------------------------------------------------------------
type Coins = [Int]

-- | All the Euro coin denominations
euroCoins :: Coins
euroCoins = [1, 2, 5, 10, 20, 50, 100, 200]

-- | generate a random list of coins
genCoins :: Gen Coins
genCoins = listOf (elements euroCoins)


-- | generate a random list of coins that adds upto a given amount
genAmount :: Int -> Gen Coins
genAmount amount
  = hillClimbing (Minimize (changeTo amount)) coinsNext 100 []

-- | change a list of coins; either add a coin or remove a coin
coinsNext :: Coins -> Gen Coins
coinsNext cs
  = oneof (addCoin : [deleteCoin | not (null cs)])
  where
    addCoin = do
      c <- elements euroCoins
      return (c:cs)
    deleteCoin = do
      c <- elements cs
      return (delete c cs)

-- Utility function: compute the leftover between
-- a target ammount and the sum of the coins
changeTo :: Int -> Coins -> Int
changeTo target cs = abs (target - sum cs)


-- | this property is wrong but passes with naive generation;
-- it states that you can't express a specific positive ammount
-- as a sum of Euro coins
prop_make_change_baseline :: Int -> Property
prop_make_change_baseline amount
  = amount>0 ==> forAll genCoins $ \cs -> sum cs /= amount

-- | using targeted generation we can express
-- that you can always express any amount using Euro coins
prop_make_change_hc ::  Int -> Property
prop_make_change_hc amount
  = amount>0 ==>
    expectFailure $
    forAll (genAmount amount) $ \cs -> sum cs /= amount

-----------------------------------------------------------------------
-- Graphs represented as lists of vertices and edges
-----------------------------------------------------------------------
data Graph
  = Graph { vertices :: [Vertex]
          , edges :: [(Vertex,Vertex)]
          } deriving Show

-- Compute the diameter of a graph, i.e.
-- the length of the longest undirected path in any component
-- Using DFF algorithm from Data.Graph to compute the support trees
-- see https://hackage.haskell.org/package/containers-0.8/docs/Data-Graph.html
diameter :: Graph -> Int
diameter g = maximum $ map depth $ Graph.dff g'
  where
    lo = minimum g.vertices
    hi = maximum g.vertices
    g' = Graph.buildG (lo,hi) g.edges

    depth :: Tree a -> Int
    depth (Node _ []) = 0
    depth (Node _ ts) = 1 + maximum (map depth ts)

-- | A straight line graph with N vertices and N-1 edges
lineGraph :: Int -> Graph
lineGraph n
  = Graph vs es
  where vs = [1..n]
        es = [(n,n+1) | n<-[1..n-1]]


-- The maximum diameter of a graph with N nodes is N-1
-- Here is a witness that this bound is tight:
-- >>> diameter (lineGraph 50)
-- 49

-- However, if you generate a random graph with N nodes and N edges
-- the diameter distributions will be much lower:

genGraph :: Int -> Gen Graph
genGraph size = do
  let vs = [1..size]
  es <- nub <$> vectorOf size (genEdge vs)
  return (Graph vs es)

genEdge :: [v] -> Gen (v,v)
genEdge vs = do
  a <- elements vs
  b <- elements vs
  return (a,b)

{-
>>> sample $ fmap diameter (genGraph 50)
4
6
8
5
7
4
4
4
8
4
5
-}

-- Let us improve generation using a search strategy.
-- We start by defining a neightbouring function for changing a
-- graph by adding or removing random edges

graphNext :: Graph -> Gen Graph
graphNext g = do
  let size = length g.edges
  let offset = 1 + floor @Float (fromIntegral size*0.05) 
  delta <- choose (1, offset)
  oneof [ removeEdges delta g
        , addEdges delta g
        ]

-- auxiliary functions
removeEdges :: Int -> Graph -> Gen Graph
removeEdges n g
  | n>0 && not (null g.edges)  = do
      e <- elements g.edges
      removeEdges (n-1) g{edges = delete e g.edges}
removeEdges _ g = return g

addEdges :: Int -> Graph -> Gen Graph
addEdges n g = do
  new <- vectorOf n (genEdge g.vertices)
  return g { edges = nub (g.edges ++ new) }

-- Let us state a slightly incorrect property:
-- | for all G, diameter(G) < |V| - 1
-- The correct property should have <= instead of <
-- However to find a counter example graph, we must generate graphs with
-- the maximal diameter.
-- Naive random generation does not produce such graph, hence testing passes.
prop_max_diameter_wrong :: Int -> Property
prop_max_diameter_wrong size
  = forAll (genGraph size) $ \g -> diameter g < size-1

-- Using targeted generation we can falsify the property
-- and obtain a counter example
prop_max_diameter_hc :: Int -> Property
prop_max_diameter_hc size
  = expectFailure $
    forAll (genGraph size >>=
            hillClimbing (Maximize diameter) graphNext 1000)
    $ \g -> diameter g < size-1


---------------------------------------------------------------------
-- The main entry point 
---------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  describe "Solving the 8-queens puzzle" $ do
    it "number of attacks in a random board" $
      property prop_measure_attacks_baseline
    it "number of attacks after 10000 iters (hill climbing)" $
      property (prop_measure_attacks_hc 10000)
    it "number of attacks after 10000 iters (simulated annealing)" $
      property (prop_measure_attacks_sa 10000)
      
  describe "Euro coins changes" $ do
    it "no change using naive generator" $
      property (prop_make_change_baseline 137)
    it "find change using targeted generator (hill climbing)" $
      property (prop_make_change_hc 137)

  describe "Maximum graph diameter conjecture" $ do
    it "Can't falsify using naive generator" $ 
       property (prop_max_diameter_wrong 50)
    it "Falsify using targeted generator (hill climbing)" $
       property (prop_max_diameter_hc 50)

  
