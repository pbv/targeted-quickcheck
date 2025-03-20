{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Test.QuickCheck
import           Data.List
import           Data.Function(on)

import qualified Data.Map as Map

import           Graph (Vertex)
import qualified Graph as Graph

import Control.Monad (replicateM)

-------------------------------------------------------------------------
-- Hill Climbing strategy
-------------------------------------------------------------------------
type Utility a = a -> Int       -- utility function
type Next m a = a -> m a        -- generate candidate neighbour

hillClimbing :: Monad m => Next m a -> Utility a -> a -> m a
hillClimbing next util s0
  = go s0 (util s0)
  where
    go s uv = do
      neighbs <- replicateM attempts (next s)
      let neighbs_uvs = [ (s',util s') | s'<- neighbs ]
      let (s',uv') = maximumBy (compare`on`snd) neighbs_uvs
      if uv' > uv then
        go s' uv'
        else
        return s

    attempts = 30


-----------------------------------------------------------------------
-- graphs represented as a lists of vertices and edges
data Graph
  = Graph { vertices :: [Vertex]
          , edges :: [(Vertex,Vertex)]
          } deriving Show

-- compute the diameter
diameter :: Graph -> Int
diameter g = go g'.vertices 
  where
    -- convert into internal representation
    g' = Graph.makeGraph g.vertices g.edges

    go :: [Vertex] -> Int
    go [] = 0
    go (v:vs) = let info = Graph.dijkstra g' v
                in maximum info.dist `max`
                   go [u | u<-vs, u`Map.notMember`info.dist]

-- number of strongly connected components
components :: Graph -> Int
components g = go g'.vertices
  where
    g' = Graph.makeGraph g.vertices g.edges

    go [] = 0
    go (v:vs) = let info = Graph.dijkstra g' v
                in 1 + go [u | u<-vs, u`Map.notMember`info.dist]


---------------------------------------------------------------
-- generate a random graph with n vertices & edges
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

neighbourInt :: Int -> Gen Int
neighbourInt base
  = let offset = floor @Double (0.05 * fromIntegral base) + 1
    in max 0 <$> choose (base-offset, base+offset)

graphNext :: Graph -> Gen Graph
graphNext g = do
  let size = length g.edges
  newsize <- neighbourInt (size)
  additional <- neighbourInt (size`div`10)
  let (removals, additions) =
        if newsize < size then
          (additional + (size-newsize), additional)
          else
          (additional, additional+(newsize-size))
  g1 <- removeEdges removals g
  addEdges additions g1



prop_max_diameter :: Int -> Property
prop_max_diameter size
  = forAll (genGraph size) $ \g -> diameter g < size`div`2

prop_max_diameter' :: Int -> Property
prop_max_diameter' size
  = forAll (genGraph size >>= hillClimbing graphNext diameter)
    $ \g -> diameter g < size`div`2


main :: IO ()
main = do
  putStrLn "prop_max_diameter"
  quickCheckWith stdArgs{maxSuccess=1000} (prop_max_diameter 50)
  putStrLn "prop_max_diameter'"
  quickCheckWith stdArgs{maxSuccess=1000} (prop_max_diameter' 50)
