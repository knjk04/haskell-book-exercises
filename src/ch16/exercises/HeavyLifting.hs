module HeavyLifting where

-- 1) Expected result:
-- Prelude> a
-- [2]
a = fmap (+1) $ read "[1]" :: [Int]

-- 2) Expected result:
-- Prelude> b
-- Just ["Hi,lol", "Hellolol"]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3) Expected result:
-- Prelude> c 1
-- -2
c = fmap (*2) (\x -> x - 2)

-- 4) Expected result:
-- Prelude> d 0
d =
  fmap ((return '1' ++) . show)
  (\x -> [x, 1..3])

-- 5) Expected result:
-- Prelude> e
-- 3693
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read ("123" ++ ) show ioi
    in (*3) changed
