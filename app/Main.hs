module Main where

import AVLTree

t1, t2, t3 :: AVLTree Int
t1 = Tree (Tree Leaf 1 (Tree Leaf 2 Leaf)) 3 Leaf
t2 = Tree (Tree Leaf 1 Leaf) 2 (Tree Leaf 3 Leaf)
t3 =
  Tree
    (Tree
      (Tree
        (makeEndNode 2)
        7
        Leaf)
      24
      (makeEndNode 32))
    37
    (Tree
      (makeEndNode 40)
      42
      (Tree
        Leaf
        42
        (makeEndNode 120)))

t4 :: AVLTree Char
t4 =
  Tree
    (Tree
      (makeEndNode 'A')
      'Y'
      (makeEndNode 'B'))
    'X'
    (makeEndNode 'C')

tll =
  Tree
    (Tree
      (Tree
        (makeEndNode 'A')
        'Z'
        (makeEndNode 'B'))
      'Y'
      (makeEndNode 'C'))
    'X'
    (makeEndNode 'D')

tlr =
  Tree
    (Tree
      (makeEndNode 'A')
      'Y'
      (Tree
        (makeEndNode 'B')
        'Z'
        (makeEndNode 'C')))
    'X'
     (makeEndNode 'D')

trl =
  Tree
    (makeEndNode 'A')
    'X'
    (Tree
      (Tree
        (makeEndNode 'B')
        'Z'
        (makeEndNode 'C'))
      'Y'
      (makeEndNode 'D'))

trr =
  Tree
    (makeEndNode 'A')
    'X'
    (Tree
      (makeEndNode 'B')
      'Y'
      (Tree
        (makeEndNode 'C')
        'Z'
        (makeEndNode 'D')))

rebalance' t = rebalance (balanceOf t) t

main :: IO ()
main = do
  putStrLn $ "LL: " ++ show tll ++ " -> " ++ show (rebalance' tll)
  putStrLn $ "LR: " ++ show tlr ++ " -> " ++ show (rebalance' tlr)
  putStrLn $ "RL: " ++ show trl ++ " -> " ++ show (rebalance' trl)
  putStrLn $ "RR: " ++ show trr ++ " -> " ++ show (rebalance' trr)
  let to15 = insertMany [1..15] Leaf
  putStrLn $ "Inserting 1..15: " ++ show to15
  putStrLn $ "Balances are: " ++ show (mapBalances to15) 
