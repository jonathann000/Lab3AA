{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree

--splitData :: String -> [a]
--splitData s = words s

testFold :: String -> AATree String
testFold s = foldl insert emptyTree (words s)

optimalHeight :: AATree a -> Int
optimalHeight t = ceiling(logBase 2 (fromIntegral((n + 1) - 1)))
  where n = size t

ratio :: AATree a -> Float
ratio t = h/h'
  where h  = fromIntegral(height t)
        h' = fromIntegral(optimalHeight t)

firstTwentyWords :: AATree String -> String 
firstTwentyWords t = unwords(take 20 (inorder t))

--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- readFile "swahili-small.txt"

  -- spl it the data into words and build an AA tree
  -- use foldl
  let tree = testFold contents
  
  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase
  putStrLn ("size: " ++ show (size tree))
  putStrLn ("height: " ++ show (height tree))
  putStrLn ("optimalHeight: " ++ show (optimalHeight tree))
  putStrLn ("ratio: " ++ show (ratio tree))
  putStrLn ("checkTree: " ++ show (checkTree tree))
  putStrLn ("firstTwentyWords: " ++ show (firstTwentyWords tree))


--------------------------------------------------------------------------------

