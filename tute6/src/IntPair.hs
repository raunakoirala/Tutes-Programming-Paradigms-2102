-- | Example representation of a pair of integers.
--
-- The focus of these exercises is to get comfortable with pattern matching
-- and working with a simple custom type.
--
-- See https://tgdwyer.github.io/haskell2/#pattern-matching
module IntPair () where

-- | An 'IntPair' contains two integers.
--
-- See https://tgdwyer.github.io/haskell2/#algebraic-data-types
data IntPair = IntPair Int Int
  deriving (Show)

-- $setup
-- >>> p1 = (IntPair 5 6)
-- >>> p2 = (IntPair 7 1)
-- >>> p3 = (IntPair 9 9)


-- | Subtract the two elements of a pair.
--
-- >>> minusIntPair p1
-- -1
--
-- >>> minusIntPair p2
-- 6
minusIntPair :: IntPair -> Int
minusIntPair (IntPair x y) = x - y

-- | Return the maximum element in a pair.
--
-- >>> maxIntPair p1
-- 6

-- >>> maxIntPair p2
-- 7

maxIntPair :: IntPair -> Int
maxIntPair (IntPair x y) = max x y

-- | Subtract two pairs together.
--
-- >>> subIntPair p1 p2
-- IntPair (-2) 5

subIntPair :: IntPair -> IntPair -> IntPair
subIntPair (IntPair x1 y1) (IntPair x2 y2) = IntPair (x1 - x2) (y1 - y2)
