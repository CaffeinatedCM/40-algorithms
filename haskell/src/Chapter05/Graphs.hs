module Chapter05.Graphs where

import Data.Graph 
import Data.List
import Data.Maybe
import Data.Tuple.Extra
import Data.Array

sample :: [(String, String, [String])]
sample = [("Amin" , "Amin" , ["Wasim", "Nick", "Mike"]),
          ("Wasim", "Wasim", ["Imran", "Amin"]),
          ("Imran", "Imran", ["Wasim", "Faras"]),
          ("Faras", "Faras", ["Imran"]),
          ("Mike" , "Mike" , ["Amin"]),
          ("Nick" , "Nick" , ["Amin"])]

sampleGraph :: (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
sampleGraph = graphFromEdges sample 

type GraphWrapper a b = (Graph, Vertex -> (a, b, [b]), b -> Maybe Vertex)

bfs :: (Graph, Vertex -> (a, b, [b]), b -> Maybe Vertex) -> b -> Maybe [a]
bfs g@(_,nodeFromVertex,vertexFromKey) k = case vertexFromKey k of
  Nothing -> Nothing
  Just n ->Just $ map (fst3 . nodeFromVertex) $ bfs' g [n] []


bfs' :: (Graph, Vertex -> (a, b, [b]), b -> Maybe Vertex) -> [Vertex] -> [Vertex] -> [Vertex]
bfs' _ [] _                                                               = []
bfs' g@(_,nodeFromVertex,vertexFromKey) (v:vs) visited | v `elem` visited = bfs' g vs visited
                                                       | otherwise        = v : bfs' g  (vs ++ keys) (v:visited)
                                                                            where (_, _, nKeys) = nodeFromVertex v
                                                                                  keys = mapMaybe vertexFromKey nKeys

dfs :: (Graph, Vertex -> (a, b, [b]), b -> Maybe Vertex) -> b -> Maybe [a]
dfs g@(_,nodeFromVertex,vertexFromKey) k = case vertexFromKey k of
  Nothing -> Nothing
  Just n ->Just $ map (fst3 . nodeFromVertex) $ dfs' g [n] []


dfs' :: (Graph, Vertex -> (a, b, [b]), b -> Maybe Vertex) -> [Vertex] -> [Vertex] -> [Vertex]
dfs' _ [] _                                                               = []
dfs' g@(_,nodeFromVertex,vertexFromKey) (v:vs) visited | v `elem` visited = dfs' g vs visited
                                                       | otherwise        = v : dfs' g  (keys ++ vs) (v:visited)
                                                                            where (_, _, nKeys) = nodeFromVertex v
                                                                                  keys = mapMaybe vertexFromKey nKeys
