-- Keshav Santhanam
-- 4/10/23
-- CS 4337.HON

--main :: IO ()
--main = return ()

-- 1) primeFactorList
primeFactorList :: Integer -> [Integer]
primeFactorList num = findFactors num 2
  where
    findFactors :: Integer -> Integer -> [Integer]
    findFactors 1 _ = [] -- 1 has no unique factors (base case)
    -- use modulus to check for possible factors and then combine them with the list
    findFactors num final | num `mod` final == 0 = final : findFactors (num `div` final) final
      | final ^ 2 > num = [num] | otherwise = findFactors num (final + 1)
      
-- 2) encode/encodeCount
encode[] = [] -- empty list case
encode(w:x) = encode' 1 w x where
  encode' num w [] = [(num, w)] -- no more characters to compare to
  -- this line compares w to other characters in y through z and sums them into num
  encode' num w (y:z) | w == y = encode' (num + 1) w z | otherwise = (num, w) : encode' 1 y z

-- 3) symmetric
-- defining tree type
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
-- input of 2 trees, ouput Boolean
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
-- validate if what you see on one branch matches the other side
mirror (Branch _ x1 y2) (Branch _ x2 y1) = mirror x2 y2 && mirror y1 x1
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
-- compare opposite branches
symmetric (Branch _ left right) = mirror left right
          
