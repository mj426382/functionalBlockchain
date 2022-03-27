-- mj426382
module HashTree where

import Hashable32
import Utils

combine :: Hash -> Hash -> Hash
combine x y = hash (x, y)

data Tree a = Nil | Node Hash (Tree a) (Tree a) | Leaf a Hash

leaf :: Hashable a => a -> Tree a
leaf x = Leaf x (hash x)

twig :: Hashable a => Tree a -> Tree a
twig Nil = Nil
twig (Leaf x h) = Node (combine h h) (Leaf x h) Nil
twig (Node h a b) = Node (combine (hash h) (hash h)) (Node h a b) Nil

node :: Hashable a => Tree a -> Tree a -> Tree a
node Nil Nil = Nil
node (Leaf x1 h1) (Leaf x2 h2) = Node (combine h1 h2) (Leaf x1 h1) (Leaf x2 h2)
node (Leaf x1 h1) (Node h2 a2 b2) = Node (combine h1 h2) (Leaf x1 h1) (Node h2 a2 b2)
node (Node h1 a1 b1) (Leaf x2 h2) = Node (combine h1 h2) (Node h1 a1 b1) (Leaf x2 h2)
node (Node h1 a1 b1) (Node h2 a2 b2) = Node (combine h1 h2) (Node h1 a1 b1) (Node h2 a2 b2)

buildTree :: Hashable a => [a] -> Tree a
buildTree [] = Nil
buildTree [v] = leaf v
buildTree list = buildRecursive (buildLeaves list)

buildRecursive :: Hashable a => [Tree a] -> Tree a
buildRecursive [] = Nil
buildRecursive [t] = t
buildRecursive list = buildRecursive (calculatePairs (buildPairs list))

calculatePairs :: Hashable a => [(Tree a, Tree a)] -> [Tree a]
calculatePairs [] = []
calculatePairs [p] = [buildNodeFromPair p]
calculatePairs (x : zs) = buildNodeFromPair x : calculatePairs zs

buildNodeFromPair :: Hashable a => (Tree a, Tree a) -> Tree a
buildNodeFromPair (Nil, Nil) = Nil
buildNodeFromPair (t, Nil) = twig t
buildNodeFromPair (Nil, t) = twig t
buildNodeFromPair (t1, t2) = node t1 t2

buildPairs :: Hashable a => [Tree a] -> [(Tree a, Tree a)]
buildPairs [] = []
buildPairs [x] = [(x, Nil)]
buildPairs (x : y : zs) = (x, y) : buildPairs zs

buildLeaves :: Hashable a => [a] -> [Tree a]
buildLeaves = map leaf

treeHash :: Tree a -> Hash
treeHash Nil = 0
treeHash (Leaf x h) = h
treeHash (Node h a b) = h

concatSpace :: Int -> String
concatSpace 0 = ""
concatSpace x = ' ' : concatSpace (x - 1)

drawTree :: Show a => Tree a -> String
drawTree t = drawPrettyTree t 0

drawPrettyTree :: Show a => Tree a -> Int -> String
drawPrettyTree Nil v = ""
drawPrettyTree (Leaf x h) v = concatSpace v ++ showHash h ++ " " ++ show x ++ "\n"
drawPrettyTree (Node h t Nil) v = concatSpace v ++ showHash h ++ " +\n" ++ drawPrettyTree t (v + 2)
drawPrettyTree (Node h Nil t) v = concatSpace v ++ showHash h ++ " +\n" ++ drawPrettyTree t (v + 2)
drawPrettyTree (Node h t1 t2) v = concatSpace v ++ showHash h ++ " -\n" ++ drawPrettyTree t1 (v + 2) ++ drawPrettyTree t2 (v + 2)

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

merklePathsValidator :: Hashable a => a -> [MerklePath] -> Maybe (MerkleProof a)
merklePathsValidator x [] = Nothing
merklePathsValidator x paths = Just (MerkleProof x (head paths))

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof x t = merklePathsValidator x (merklePaths x t)

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths x Nil = []
merklePaths x (Leaf y z) = [[] | hash x == hash y]
merklePaths l (Node h Nil Nil) = []
merklePaths l (Node h t1 Nil) = map (\x -> Left (treeHash t1) : x) (merklePaths l t1)
merklePaths l (Node h Nil t2) = map (\x -> Right (treeHash t2) : x) (merklePaths l t2)
merklePaths l (Node h t1 t2) = map (\x -> Left (treeHash t2) : x) (merklePaths l t1) ++ map (\x -> Right (treeHash t1) : x) (merklePaths l t2)

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h p = h == countCurrentHashProof p

countCurrentHashProof :: Hashable a => MerkleProof a -> Hash
countCurrentHashProof (MerkleProof x [Left y]) = combine (hash x) y
countCurrentHashProof (MerkleProof x [Right y]) = combine y (hash x)
countCurrentHashProof (MerkleProof x ((Left y) : z)) = combine (countCurrentHashProof (MerkleProof x z)) y
countCurrentHashProof (MerkleProof x ((Right y) : z)) = combine y (countCurrentHashProof (MerkleProof x z))

showMerklePath :: MerklePath -> String
showMerklePath [] = ""
showMerklePath [Left h] = "<" ++ showHash h
showMerklePath [Right h] = ">" ++ showHash h
showMerklePath (x : z) = showMerklePath [x] ++ showMerklePath z

instance Show a => Show (MerkleProof a) where
    showsPrec p (MerkleProof l m) = showParen(p > 1) (showString "MerkleProof " . showsPrec 11 l . showString (" " ++ showMerklePath m))
