module Ch14Exercises where

import Test.Hspec

-- from ch8exercises.hs
-- multiplies two numbers together using recursive summation
-- function has been modified as a result of failing spec cases
mult :: Integral a => a -> a -> a
mult 0 _ = 0 -- 0 x _ = 0
mult 1 n = n -- 1 x n = n
mult _ 0 = 0 -- _ x 0 = 0
mult m 1 = m -- m x 1 = m
mult m n | m < 0 && n < 0 = go (negate m) (negate n) -- two negatives make a positive
          | m < 0          = negate $ go (negate m) n -- Only m is negative: result is negative
          | n < 0          = negate $ go m (negate n) -- Only n is negative: result is negative
          | otherwise      = go n m -- Both operands are positive
  where go m n = m + mult m (n - 1)

main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "3 times 4 is 12" $ do
      mult 3 4 `shouldBe` 12
    it "3 times 0 is 0" $ do
      mult 3 0 `shouldBe` 0
    it "3 times 1 is 3" $ do
      mult 3 1 `shouldBe` 3
    it "3 times -4 is -12" $ do
      mult 3 (-4) `shouldBe` (-12)
