import Data.List (sort, group)

main :: IO ()
main = partOne >> partTwo

partOne :: IO ()
partOne = interact (show . partOne' . lines)

partOne' :: [String] -> Int
partOne' strs
  = (sum two) * (sum three)
    where
      (two, three) = unzip $ map twoOrThreeOcs strs

twoOrThreeOcs :: String -> (Int, Int)
twoOrThreeOcs str
  = (two, three)
    where
      grouped = group $ sort str
      two     = if any ((== 2) . length) grouped then 1 else 0
      three   = if any ((== 3) . length) grouped then 1 else 0
    
  
partTwo :: IO ()
partTwo = interact (partTwo' . lines)

partTwo' :: [String] -> String
partTwo' strs = similarityOne strs strs strs

similarityOne :: [String] -> [String] -> [String] -> String
similarityOne [] (t:ts) og = similarityOne og ts og
similarityOne (s:ss) (t:ts) og
  = if differences == 1 then similarities else similarityOne ss (t:ts) og
    where
      bits         = map (uncurry (==)) $ zip s t
      differences  = (length . (filter not)) bits
      similarities = map fst $ filter snd $ zip s bits

