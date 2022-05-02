{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree

testFold :: String -> AATree String
testFold s = foldl insert emptyTree (words s)

ratio :: Float  -> Float  -> Float
ratio h h' = h/h'
--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- readFile "swahili-small.txt"
  --getContents

  -- split the data into words and build an AA tree
  -- use foldl
  let tree = testFold contents

  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase
  putStrLn ("Size: " ++ show(size tree))
  putStrLn ("Height: " ++ show(height tree))
  putStrLn ("Optimal height: " ++ show (ceiling (logBase 2(fromIntegral(size tree + 1)) - 1)))
  putStrLn ("Height / Optimal height: " ++ show (ratio (fromIntegral(height tree)) (logBase 2(fromIntegral(size tree + 1)) - 1)))
  print (checkTree tree)
  putStrLn (unwords (take 20 (inorder tree)))

--------------------------------------------------------------------------------