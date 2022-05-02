{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where
--------------------------------------------------------------------------------

-- AA search trees
data AATree a
  = Empty
  | Node {
  level :: Int,
  left :: AATree a,
  value :: a,
  right :: AATree a }
  deriving (Eq, Show, Read)

emptyTree :: AATree a
emptyTree = Empty

get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get input (Node _ l a r)
  | input == a = Just a
  | input < a  = get input l
  | otherwise  = get input r

split :: AATree a -> AATree a 
split n@(Node i a v (Node ri b rv rr))
  | not (rightGrandchildOK n) = Node (ri+1) (Node i a v b) rv rr
split n = n

skew :: AATree a -> AATree a
skew n@(Node i (Node li ll lv lr) v r)
  | not (leftChildOK n) = Node li ll lv (Node i lr v r)
skew n = n

insert :: Ord a => AATree a -> a -> AATree a
insert n@(Node i l v r) input =
  if input == v
    then n
    else if input < v
      then split(skew(Node i (insert l input) v r))
      else split(skew(Node i l v (insert r input)))
insert Empty input = Node 1 Empty input Empty 


inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node _ Empty x Empty) = [x]
inorder (Node _ Empty x r)     = x : inorder r
inorder (Node _ l x r)         = inorder l ++ (x : inorder r)

-- O(1)
size :: AATree a -> Int
size Empty = 0
size n = length (inorder n)

-- O(log n)
height :: AATree a -> Int
height Empty = 0
height (Node _ l _ r) 
  | size r > size l  = 1 + height r 
  | otherwise        = 1 + height l


--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants
--(O)
checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

--O(n)
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs)
  | x < y     = isSorted (y:xs)
  | otherwise = False

-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant

--O(1)
checkLevels :: AATree a -> Bool
checkLevels Empty = True
checkLevels node = leftChildOK node &&
                   rightChildOK node &&
                   rightGrandchildOK node

--O(1)
isEmpty :: AATree a -> Bool
isEmpty Empty = True
isEmpty _     = False

--O(1)
leftSub :: AATree a -> AATree a
leftSub Empty = Empty
leftSub (Node _ l _ _) = l

--O(1)
rightSub :: AATree a -> AATree a
rightSub Empty = Empty
rightSub (Node _ _ _ r) = r

--O(1)
leftChildOK :: AATree a -> Bool
leftChildOK (Node i (Node li _ _ _) _ _) = li < i
leftChildOK _ = True

--O(1)
rightChildOK :: AATree a -> Bool
rightChildOK (Node i _ _ (Node ir _ _ _)) = ir <= i
rightChildOK _ = True

--O(1)
rightGrandchildOK :: AATree a -> Bool
rightGrandchildOK (Node i _ _ (Node _ _ _ (Node rri _ _ _))) = i > rri
rightGrandchildOK _ = True

--------------------------------------------------------------------------------

