main :: IO ()
main = partTwo

partOne :: IO ()
partOne = interact (show . sum . (map reallyRead) . lines)

reallyRead :: String -> Int
reallyRead ('+':t) = read t
reallyRead str     = read str

partTwo :: IO ()
partTwo = interact (show . c2F . (map reallyRead) . lines)
  
c2F :: [Int] -> Int
c2F list = catch2Frequencies 0 [] list list

catch2Frequencies :: Int -> [Int] -> [Int] -> [Int] -> Int
catch2Frequencies oldFrequency catched [] og = catch2Frequencies oldFrequency catched og og
catch2Frequencies oldFrequency catched (change:inL) og
  | newFrequency `elem` catched = newFrequency
  | otherwise                   = catch2Frequencies newFrequency (newFrequency:catched) inL og
  where
    newFrequency = oldFrequency + change