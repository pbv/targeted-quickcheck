{-# LANGUAGE OverloadedRecordDot #-}
--------------------------------------------------------------------
-- Dijkstra's algorithm 
-- adapted from Well-Typed Haskell Unfolder presentation:
-- https://github.com/well-typed/unfolder/blob/main/episode020-dijkstras-shortest-paths/Dijkstra.hs
-- modified to use an intmap as a priority queue instead of a list of vertices

module Graph where

import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)

import           Data.List (foldl')

type Vertex = Int
type Dist = Int   -- maxBound for infinity

-- internal graph representation
data Graph =
  MkGraph
    { vertices   :: [Vertex]
    , neighbours :: Vertex -> [Vertex]
    }

makeGraph :: [Vertex] -> [(Vertex,Vertex)] -> Graph
makeGraph verts edges
  = MkGraph
    { vertices = verts
    , neighbours = \v -> IntMap.findWithDefault undefined v g
    }
    where g = IntMap.fromList [(u,vs) | u<-verts,
                               let vs = [v' | (v,v')<-edges, u==v]]
    
data Info  =
  MkInfo
    { dist :: !(Map Vertex Dist)     -- not in map: INFINITY
    , prev :: !(Map Vertex Vertex)  
    , pqueue :: !(PQ Vertex)
    }
  deriving Show

distanceOf :: Info -> Vertex -> Dist
distanceOf info v 
  =  Map.findWithDefault maxBound v info.dist 

dijkstra :: Graph -> Vertex -> Info 
dijkstra graph source =
  let
    loop :: Info -> Info 
    loop info =
      case minView info.pqueue of
        Nothing ->
          -- we're done!
          info
        Just (u, pq') ->
          let du = distanceOf info u
          in 
          if du < maxBound then
            let
                update :: Info -> Vertex -> Info 
                update i v =
                  let
                    alt = du + 1
                  in
                     if alt < distanceOf i v then
                        MkInfo
                          (Map.insert v alt i.dist)
                          (Map.insert v u i.prev)
                          (addWithPri v alt i.pqueue)
                     else
                        i 
                info' =
                  foldl' update info{pqueue=pq'} (graph.neighbours u)
              in
                loop info'
            else
              info

  in
    loop 
      (MkInfo
        { dist = Map.singleton source 0
        , prev = Map.empty
        , pqueue = IntMap.singleton 0 [source]
        }
      )


shortestPath :: Info -> Vertex -> [Vertex]
shortestPath info = go
  where
    go v = v : case Map.lookup v info.prev of
              Just v' -> go v'
              Nothing -> []

-- utilities -----------------------------------------------------------
-- use IntMap as a replacement for priority queues
-- (big speedup over naive searching through lists)

type PQ a = IntMap [a]

minView :: PQ a -> Maybe (a, PQ a)
minView pq
  = case IntMap.lookupMin pq of
      Nothing -> Nothing
      Just (_, []) -> Nothing
      Just (_, x:xs) ->
        let pq' = if null xs then
                    IntMap.deleteMin pq
                  else
                    IntMap.updateMin (\_ -> Just xs) pq                    
        in 
          Just (x, pq')
               
addWithPri :: a -> Int -> PQ a -> PQ a
addWithPri x p pq
  = IntMap.insertWith (++) p [x] pq

