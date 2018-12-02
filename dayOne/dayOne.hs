import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace
main :: IO ()
main = partOne >> partTwo

partOne :: IO ()
partOne = interact (show . sum . (map reallyRead) . lines)

reallyRead :: String -> Int
reallyRead ('+':t) = read t
reallyRead str     = read str

partTwo :: IO ()
partTwo = interact (show . c2F . (map reallyRead) . lines)
  
c2F :: [Int] -> Int
c2F list = catch2Frequencies 0 S.empty list list

catch2Frequencies' :: Int -> (S.Set Int) -> [Int] -> [Int] -> Int -> Int
catch2Frequencies' oldFrequency catched [] og n = trace (show n) $ catch2Frequencies' oldFrequency catched og og (n + 1)
catch2Frequencies' oldFrequency catched (change:inL) og n
  | newFrequency `S.member` catched = newFrequency
  | otherwise                       = catch2Frequencies' newFrequency (S.insert newFrequency catched) inL og n
  where
    newFrequency = oldFrequency + change

catch2Frequencies :: Int -> (S.Set Int) -> [Int] -> [Int] -> Int
catch2Frequencies oldFrequency catched [] og = catch2Frequencies oldFrequency catched og og
catch2Frequencies oldFrequency catched (change:inL) og
  | newFrequency `S.member` catched = newFrequency
  | otherwise                       = catch2Frequencies newFrequency (S.insert newFrequency catched) inL og
  where
    newFrequency = oldFrequency + change

--- Improving Part Two

actuallyWillRepeat :: [Int] -> [Int]
actuallyWillRepeat xs = tail $ scanl (+) 0 xs

offset :: [Int] -> Int
offset l = sum l 

initMap :: M.Map Int (S.Set Int)
initMap = M.empty

modMap :: [Int] -> Int -> M.Map Int (S.Set Int) -> M.Map Int (S.Set Int)
modMap [] offset map = map
modMap (i:ints) offset map 
  = if M.member (mod i offset) map
    then
      modMap ints offset (M.insertWith (S.union) (mod i offset) (S.singleton i) map) 
    else
      modMap ints offset (M.insert (mod i offset) (S.singleton i) map)

