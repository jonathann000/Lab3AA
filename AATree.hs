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

-- O(1)
emptyTree :: AATree a
emptyTree = Empty

-- O(log n)
get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get input (Node _ l a r)
  | input == a = Just a
  | input < a  = get input l
  | otherwise  = get input r

-- O(1)
split :: AATree a -> AATree a 
split n@(Node i a v (Node ri b rv rr))
  | not (rightGrandchildOK n) = Node (ri+1) (Node i a v b) rv rr
split n = n

-- O(1)
skew :: AATree a -> AATree a
skew n@(Node i (Node li ll lv lr) v r)
  | not (leftChildOK n) = Node li ll lv (Node i lr v r)
skew n = n

-- O(log n)
{- I första fallet kollar vi om värdet på den nya noden är lika med värdet på noden i trädet
i detta fallet ignoreras inserten, annars kollar vi om den är lägre eller större och traverserar 
trädet tills vi kommer till en tom plats och lägger in den nya noden där. Efter detta skewar och splittar
vi för att se till att invarianten håller, logiken för att se om ett träd behöver skewas/splittas finns i 
respektive funktion och inte i insert. 
-}
insert :: Ord a => AATree a -> a -> AATree a
insert n@(Node i l v r) input =
  if input == v
    then n
    else if input < v
      then split(skew(Node i (insert l input) v r))
      else split(skew(Node i l v (insert r input)))
insert Empty input = Node 1 Empty input Empty 

-- O(n)
{- Vi traverserar listan neråt till den mest vänstra noden och längs vägen kallar rekursivt
funktionen på samtliga högerbarn och lägger till elementen i den ordningen.
-}
inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node _ Empty x Empty) = [x]
inorder (Node _ Empty x r)     = x : inorder r
inorder (Node _ l x r)         = inorder l ++ (x : inorder r)

-- O(n)
size :: AATree a -> Int
size Empty = 0
size n     = length (inorder n)
{-- 
height :: AATree a -> Int
height Empty = 0
height (Node _ _ _ r) = 1 + height r

--O(n^2)
height :: AATree a -> Int
height Empty = 0
height n@(Node _ l _ r) 
  | aaHeight r == aaHeight n    = 1 + height r 
  | otherwise                   = 1 + height l
  where aaHeight (Node i _ _ _) = i
        aaHeight Empty          = 0
-}
height :: AATree a -> Int
height Empty = 0
height (Node _ l _ r) = 1 + max (height l) (height r)


--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants
-- O(1)
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
isSorted [_]      = True
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

