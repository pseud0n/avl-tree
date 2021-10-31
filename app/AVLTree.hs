module AVLTree
  ( minimalNodeCount -- minimum number of nodes for given height
  , minimalNodeCount' -- memoised minimalNodeCount
  , height -- how tall tree is
  , search -- if tree contains element
  , balanceOf -- balance of a tree
  , infixList -- tree to list, infix
  , prefixList -- tree to list, prefix
  , postfixList -- tree to list, postfix
  , mapBalances -- Tree of balance of each node
  , insert -- add value to tree
  , insertMany -- add several values from list into tree
  , rebalance
  , AVLTree (..) -- main data type & constructors
  , makeEndNode -- new tree from value
  , rotateLeft
  , rotateRight
) where

import Data.Maybe (isJust)
import Control.Monad.Writer
import Control.Monad.State

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

data AVLTree a
  = Leaf
  | Tree (AVLTree a) a (AVLTree a)

data AVLTreeZipper = L | R deriving (Show)

instance (Show a) => Show (AVLTree a) where
  show Leaf = "⊥"
  show (Tree Leaf v Leaf) = show v
  show (Tree l v r) = "(" ++ show l ++ "←" ++ show v ++ "→" ++ show r ++ ")"

instance (Eq a) => Eq (AVLTree a) where
  Leaf == Leaf = True
  (Tree l v r) == (Tree l' v' r') = v == v' && l == l' && r == r'
  _ == _ = False

-- Empty tree has height -1, single node has height zero
-- Height is number of levels below root
minimalNodeCount :: Int -> Integer
minimalNodeCount (-1) = 0
minimalNodeCount 0 = 1
minimalNodeCount height = 1 + minimalNodeCount (height - 1) + minimalNodeCount (height - 2)

minimalNodeCount' :: Int -> Integer -- Significantly faster
minimalNodeCount' = (map minimalNodeCount'Aux [(-1)..] !!) . (+1)
  where
    minimalNodeCount'Aux (-1) = 0
    minimalNodeCount'Aux 0 = 1
    minimalNodeCount'Aux height = 1 + minimalNodeCount' (height - 1) + minimalNodeCount' (height - 2)

height :: AVLTree a -> Int
height Leaf = -1
height (Tree l _ r) = 1 + max (height l) (height r)

search :: Eq a => a -> AVLTree a -> Bool
search _ Leaf = False
search x (Tree l v r) = v == x || search x l || search x r

balanceOf :: AVLTree a -> Int
balanceOf Leaf = 0
balanceOf (Tree l _ r) = height l - height r

isAVL :: AVLTree a -> Bool
isAVL t = isJust $ balanceIfAVL t
  where
    balanceIfAVL :: AVLTree a -> Maybe Int
    balanceIfAVL Leaf = Just 0
    balanceIfAVL (Tree l _ r) = (-) <$> (maybeInRange =<< balanceIfAVL l) <*> (maybeInRange =<< balanceIfAVL r)
    -- Find difference between l & r
    -- If difference out of range, make into Nothing (stop calculating)

    maybeInRange :: Int -> Maybe Int
    maybeInRange x
      | x < -1 || x > 1 = Nothing
      | otherwise = Just x

infixList :: AVLTree a -> [a]
infixList Leaf = []
infixList (Tree l v r) = infixList l ++ v : infixList r

prefixList :: AVLTree a -> [a]
prefixList Leaf = []
prefixList (Tree l v r) = v : infixList l ++ infixList r

postfixList :: AVLTree a -> [a]
postfixList Leaf = []
postfixList (Tree l v r) = infixList l ++ infixList r ++ [v]

mapBalances :: AVLTree a -> AVLTree Int
mapBalances Leaf = Leaf :: AVLTree Int
mapBalances (Tree l _ r) = Tree (mapBalances l) (height l - height r) (mapBalances r)

reachedBottom, reachedTop :: a
reachedBottom = error "Reached bottom of tree"
reachedTop = error "Reached top of tree"

fromCrumbs :: [AVLTreeZipper] -> AVLTree a -> AVLTree a
fromCrumbs [] t = t
fromCrumbs (L:cs) (Tree l _ _) = fromCrumbs cs l
fromCrumbs (R:cs) (Tree _ _ r) = fromCrumbs cs r
fromCrumbs _ Leaf = reachedBottom

data Crumb a = GoneLeft a (AVLTree a) | GoneRight a (AVLTree a)
  deriving Show

goLeft :: AVLTree a -> [Crumb a] -> (AVLTree a, [Crumb a])
goLeft Leaf _ = reachedBottom
goLeft (Tree l v r) cs = (l, GoneLeft v r:cs)

goRight :: AVLTree a -> [Crumb a] -> (AVLTree a, [Crumb a])
goRight Leaf _ = reachedBottom
goRight (Tree l v r) cs = (r, GoneRight v l : cs)

goUp :: AVLTree a -> [Crumb a] -> (AVLTree a, [Crumb a])
goUp _ [] = reachedTop
goUp l (GoneLeft v r  : cs) = ( Tree l v r, cs)
goUp r (GoneRight v l : cs) = ( Tree l v r, cs)

insert' :: Ord a => a -> AVLTree a -> Writer [Crumb a] (AVLTree a)
insert' x Leaf = return $ makeEndNode x
insert' x t@(Tree l v r)
  | x <= v = do
    tell $ snd $ goLeft t []
    leftInsert <- insert' x l
    return $ Tree leftInsert v r
  | otherwise = do
    tell $ snd $ goRight t []
    rightInsert <- insert' x l
    return $ Tree l v rightInsert

noBalanceInsert :: Ord a => a -> AVLTree a -> AVLTree a
noBalanceInsert x Leaf = makeEndNode x
noBalanceInsert x t@(Tree l v r)
  | x <= v = Tree (noBalanceInsert x l) v r
  | otherwise = Tree l v (noBalanceInsert x r)

insert'' :: Ord a => a -> AVLTree a -> State (AVLTree a, [Crumb a]) (AVLTree a)
insert'' x Leaf = do
  let newTree = makeEndNode x
  (_, crumbs) <- get
  put (newTree, crumbs)
  return newTree
insert'' x t@(Tree l v r)
  | x <= v = do
    currentState <- get
    let (leftTree, crumbs) = uncurry goLeft currentState
    -- works out left tree and breadcrumbs
    put (leftTree, crumbs)
    insert'' x leftTree
  | otherwise = do
    currentState <- get
    let (rightTree, crumbs) = uncurry goRight currentState
    -- works out left tree and breadcrumbs
    put (rightTree, crumbs)
    insert'' x rightTree

getBalance h c = ownBalance
  where
    ownBalance = case c of
      (GoneLeft v rTree) -> h - height rTree
      (GoneRight v lTree) -> height lTree - h

insert :: (Ord a, Show a) => a -> AVLTree a -> AVLTree a
insert x Leaf = makeEndNode x
insert x t = (\(_,result,_) -> result) $
  uncurry (reconstructUntilUnbalanced 0) $ -- 0 is balance of end node
    execState (insert'' x t) (t, [])

insertMany :: (Show a, Ord a) => [a] -> AVLTree a -> AVLTree a
--insertMany [] t = t
--insertMany (x:xs) t = insertMany xs $ insert x t
insertMany xs t = foldl (flip insert) t xs

rebalance :: Show a => Int -> AVLTree a -> AVLTree a
rebalance balance t
  | balance <= -2 = rebalanceR t
  | balance >= 2 = rebalanceL t
  | otherwise = t

rebalanceError :: Show a => AVLTree a -> b
rebalanceError t = error $ "Cannot rebalance" ++ show t

rebalanceR tx@(Tree a x ty@(Tree Tree {} y (Tree Leaf _ Leaf))) = rotateLeft $ Tree a x (rotateRight ty) --rl
rebalanceR tx@(Tree a x ty@(Tree b y tz@(Tree c z d))) = rotateLeft tx -- rr
rebalanceR t = rebalanceError t

rebalanceL tx@(Tree (Tree (Tree a z b) y (Tree Leaf _ Leaf)) x d) = rotateRight tx -- ll
rebalanceL tx@(Tree ty@(Tree a y Tree {}) x d) = rotateRight $ Tree (rotateLeft ty) x d --lr
rebalanceL t = rebalanceError t

reconstructTree :: AVLTree a -> [Crumb a] -> (AVLTree a, [Crumb a])
reconstructTree t [] = (t, [])
reconstructTree t cs = uncurry reconstructTree $ goUp t cs

reconstructUntilUnbalanced :: Show a => Int -> AVLTree a -> [Crumb a] -> (Int, AVLTree a, [Crumb a])
reconstructUntilUnbalanced h t [] = (h, t, [])
reconstructUntilUnbalanced h t cs =
  if abs ownBalance > 1 then -- Left-heavy, use rebalanceL
    --error $ show $ goUp t (c:cs)
    (\(x,y) -> (0, x, y)) $ reconstructTree (rebalanceR ownTree) crumbs
  else
    reconstructUntilUnbalanced (h + 1) ownTree crumbs
  where
    ownBalance = case head cs of
      (GoneLeft v rTree) -> h - height rTree
      (GoneRight v lTree) -> height lTree - h
    (ownTree, crumbs) = goUp t cs
    -- ownBalance: the difference between balances of left and right
    -- ownTree: The constructed tree
    -- crumbs: What to follow to rebuild entire tree

instance Functor AVLTree where
  fmap _ Leaf = Leaf
  fmap f (Tree l v r) = Tree (fmap f l) (f v) (fmap f r)

instance Applicative AVLTree where
  pure x = infiniteAVLTree -- not a real AVL tree!
    where infiniteAVLTree = Tree infiniteAVLTree x infiniteAVLTree
    -- Behaviour modelled off of ZipList

  -- pure f <*> x = f <$> x
  -- left argument is never a leaf
  (Tree lgs g rgs) <*> (Tree lxs x rxs) = Tree (lgs <*> lxs) (g x) (rgs <*> rxs)
  _ <*> _ = Leaf

rotateRight :: AVLTree a -> AVLTree a
rotateRight (Tree (Tree a y b) x c) = Tree a y (Tree b x c)
rotateRight _ = undefined

rotateLeft :: AVLTree a -> AVLTree a
rotateLeft (Tree a y (Tree b x c)) = Tree (Tree a y b) x c
rotateLeft _ = undefined

makeEndNode :: a -> AVLTree a
makeEndNode v = Tree Leaf v Leaf

--demo = do
--  let tree = insertMany [1..5] Leaf
--  print tree
--  let h = 0
--  let (t, cs) = execState (insert'' 6 tree) (tree, [])
--  print (h, t, cs)
--  let ownBalance = case head cs of
--                     (GoneLeft v rTree) -> h - height rTree
--                     (GoneRight v lTree) -> height lTree - h
--  let (ownTree, crumbs) = goUp t cs
--  print (ownBalance, ownTree, crumbs)
--  putStrLn "Okay, done"
--  let h' = h + 1
--  let cs' = crumbs
--  let t' = ownTree
--  print (h', t', cs')
--  let ownBalance' = case head cs' of
--                     (GoneLeft v rTree) -> h' - height rTree
--                     (GoneRight v lTree) -> height lTree - h'
--  let (ownTree', crumbs') = goUp t' cs'
--  print (ownBalance', ownTree', crumbs')
--
--  putStrLn "Okay, done"
--  let h'' = h' + 1
--  let cs'' = crumbs'
--  let t'' = ownTree'
--  print (h'', t'', cs'')
--  let ownBalance'' = case head cs'' of
--                     (GoneLeft v rTree) -> h'' - height rTree
--                     (GoneRight v lTree) -> height lTree - h''
--  let (ownTree'', crumbs'') = goUp t'' cs''
--  print (ownBalance'', ownTree'', crumbs'')
--  print $ rebalance ownBalance'' ownTree''
