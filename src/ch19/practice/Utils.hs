module Web.Shipping.Utils ((<||>)) where

import Control.Applicative (liftA2)

-- logical disjunction as an infix operator that will be lifted over some context
-- Same as:
-- > f 9001 = True; f _ = False
-- > g 42 = True; g _ = False

-- > f 42
-- False

-- > f 9001
-- True

-- > g 9001
-- False

-- > g 42
-- True

-- > (\n -> f n || g n) 0  
-- False
-- > (\n -> f n || g n) 9001
-- True

-- Cleaner with this operator:
-- > (f <||> g) 0
-- False
-- > (f <||> g) 9001
-- True
(<||>) :: (a -> Bool)
       -> (a -> Bool)
       -> a
       -> Bool
(<||>) = liftA2 (||)
