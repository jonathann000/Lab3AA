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



-- You may find it helpful to define
--   split :: AATree a -> AATree a
--   skew  :: AATree a -> AATree a
-- and call these from insert.

split :: AATree a -> AATree a 
split s@(Node i a x (Node yi b y z@(Node zi _ _ _))) 
  | i == zi = Node (yi+1) (Node i a x b) y z
split n = n

skew :: AATree a -> AATree a
skew (Node i (Node xi a x b) y c)
  | i == xi = Node xi a x (Node i b y c)
skew n = n


insert :: Ord a => a -> AATree a -> AATree a
insert input t@(Node i l v r)
  | input == v = t
  | input < v = fixIt (Node i (insert input l) v r)
  | otherwise = fixIt (Node i l v (insert input r))
  where fixIt = split . skew
insert input Empty = Node 1 Empty input Empty


inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node i Empty x Empty) = [x]
inorder (Node i Empty x r)     = x : inorder r 
inorder (Node i l x r)         = (inorder l ++ (x : inorder r))

size :: AATree a -> Int
size Empty = error "Not tree stupid"
size (Node _ _ x _) = x 

height :: AATree a -> Int
height Empty = 0
height (Node i _ _ _) = i

--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
isSorted :: Ord a => [a] -> Bool
isSorted = error "isSorted not implemented"

-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant
checkLevels :: AATree a -> Bool
checkLevels = error "checkLevels not implemented"

isEmpty :: AATree a -> Bool
isEmpty = error "isEmpty not implemented"

leftSub :: AATree a -> AATree a
leftSub = error "leftSub not implemented"

rightSub :: AATree a -> AATree a
rightSub = error "rightSub not implemented"

leftChildOK :: AATree a -> Bool
leftChildOK (Node i (Node li _ _ _) _ _) = li < i
leftChildOK _ = True

rightChildOK :: AATree a -> Bool
rightChildOK (Node i _ _ (Node ir _ _ _)) = ir <= i
rightChildOK _ = True

rightGrandchildOK :: AATree a -> Bool
rightGrandchildOK (Node i _ _ (Node _ _ _ (Node rri _ _ _))) = i > rri
rightGrandchildOK _ = True

--------------------------------------------------------------------------------

