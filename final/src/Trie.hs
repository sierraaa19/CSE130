-------------------------------------------------------------------------------
-- DO NOT MODIFY THIS SEGMENT
-------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Trie where

import           Prelude hiding (words)
import qualified Test.QuickCheck as QC
import qualified Data.List       as L

-- | A trie node stores whether it is terminal + zero or more edges
data Trie = Node Bool [Edge]
  deriving (Eq, Show)

-- | An edge is a pair of a character and a child trie
type Edge = (Char, Trie)

-- | The sorted trie that stores "he", "him", "his", "she", "her"
pronounsSorted = Node False [
                  ('h', Node False [
                          ('e', Node True [            -- he
                                  ('r', Node True [])  -- her
                                ]),
                          ('i', Node False [
                                  ('m', Node True []), -- him
                                  ('s', Node True [])  -- his
                                ])
                        ]),
                  ('s', Node False [
                          ('h', Node False [
                                  ('e', Node True [])  -- she
                                ])
                        ])
                ]

-- | The empty trie
empty :: Trie
empty = Node False []

-- | The trie that stores only the empty string
epsilon :: Trie
epsilon = Node True [] 

-- | All words stored in a trie
words :: Trie -> [String]
words (Node False []        ) = []
words (Node True  []        ) = [""]
words (Node b     ((k,t):es)) = [k:w | w <- words t] ++ words (Node b es)

-- | Words with a given prefix stored in a trie
withPrefix :: String -> Trie -> [String]
withPrefix []     t           = words t
withPrefix _      (Node _ []) = []
withPrefix (c:cs) (Node b ((k,t):es))
  -- Found an outgoing edge labeled 'c': go down that edge:
  | k == c                    = [k:w | w <- withPrefix cs t]
  -- NEW: current edge's label is less than 'c': check the remaining edges:
  | k < c                     = withPrefix (c:cs) (Node b es)
  -- NEW: current edge's label is already greater than 'c', so there is no 'c' edge:
  | otherwise                 = []

-------------------------------------------------------------------------------
-- Task 2.1: Insert
-------------------------------------------------------------------------------

-- | Insert a new string into a trie, keeping edges sorted.
insert :: String -> Trie -> Trie
insert [] (Node _ edges) = Node True edges
insert word@(c:cs) (Node b edges) = Node b (insertEdge word edges)
  where
    insertEdge :: String -> [Edge] -> [Edge]
    insertEdge w [] = [(c, insert cs empty)]
    insertEdge w@(c':cs') ((k,t):es)
      | c' < k    = (c', insert cs' empty) : (k,t) : es
      | c' == k   = (k, insert cs' t) : es
      | otherwise = (k,t) : insertEdge w es

-------------------------------------------------------------------------------
-- Task 2.2: Build with HOF
-------------------------------------------------------------------------------

-- | Insert all values from a list into a trie
build :: [String] -> Trie
build = foldr insert empty

-------------------------------------------------------------------------------
-- Task 2.3: Same elements
-------------------------------------------------------------------------------

-- | Do two lists contain the same set of elements?
sameElems :: Eq a => [a] -> [a] -> Bool
sameElems xs ys = null $ (L.nub xs L.\\ L.nub ys) ++ (L.nub ys L.\\ L.nub xs)

-------------------------------------------------------------------------------
-- Task 2.4: Checking sortedness
-------------------------------------------------------------------------------

-- | Is the given trie sorted?
-- | I.e., are the labels on the outgoing edges of each node *strictly* increasing?
sortedTrie :: Trie -> Bool
sortedTrie (Node _ edges) = checkEdges edges
  where
    checkEdges [] = True
    checkEdges [_] = True
    checkEdges ((c1, _):rest@((c2, _):_))
      | c1 < c2 = checkEdges rest
      | otherwise = False

-------------------------------------------------------------------------------
-- Task 2.5: Checking withPrefix
-------------------------------------------------------------------------------

-- | Does `withprefix p t` return those and only those words from `words t` that start with `p`?
withPrefixCorrect :: String -> Trie -> Bool
withPrefixCorrect p t = sameElems (withPrefix p t) pWords
  where 
    pWords = filter (isPrefix p) (words t)
    isPrefix :: String -> String -> Bool
    isPrefix [] _ = True
    isPrefix _ [] = False
    isPrefix (x:xs) (y:ys)
      | x == y = isPrefix xs ys
      | otherwise = False


-------------------------------------------------------------------------------
-- | QuickCheck: DO NOT MODIFY THIS SEGMENT
-------------------------------------------------------------------------------

-- | A trie built from `xs` contains all `xs`
prop_contains_elts :: [String] -> Bool
prop_contains_elts xs = let t = build xs in sameElems xs (words t)

-- | A trie built from `xs` is sorted
prop_sorted :: [String] -> Bool
prop_sorted xs = let t = build xs in sortedTrie t

-- | Searching by prefix returns exactly the words with a given prefix
prop_with_prefix_correct :: [String] -> String -> Bool
prop_with_prefix_correct xs prefix = let t = build xs in withPrefixCorrect prefix t

quickCheck :: (QC.Testable prop) => prop -> IO ()
quickCheck = QC.quickCheck
