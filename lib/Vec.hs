module Vec where

--------------------
-- Vector         --
--------------------

data V2 a = V2 a a deriving (Eq, Show)

instance (Ord a) => Ord (V2 a) where
  compare (V2 x1 y1) (V2 x2 y2) =
    case compare y1 y2 of
      EQ -> compare x1 x2
      other -> other

instance (Num a) => Num (V2 a) where
  (V2 x1 y1) + (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
  (V2 x1 y1) - (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
  (V2 x1 y1) * (V2 x2 y2) = V2 (x1 * x2) (y1 * y2)

  abs (V2 x y) = V2 (abs x) (abs y)
  signum (V2 x y) = V2 (signum x) (signum y)
  negate (V2 x y) = V2 (negate x) (negate y)

  fromInteger = error "unable to create vector from integer"

manhattanDistance :: (Num a) => V2 a -> V2 a -> a
manhattanDistance (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

--------------------
-- Neighbor       --
--------------------

directNeighborOffsets :: [V2 Int]
directNeighborOffsets =
  [ V2 (-1) 0,
    V2 0 1,
    V2 0 (-1),
    V2 1 0
  ]

diagonalNeighborOffsets :: [V2 Int]
diagonalNeighborOffsets =
  [ V2 (-1) 1,
    V2 (-1) (-1),
    V2 1 1,
    V2 1 (-1)
  ]

neighbors' :: [V2 Int] -> V2 Int -> [V2 Int]
neighbors' n p = map (+ p) n

directNeighbors :: V2 Int -> [V2 Int]
directNeighbors = neighbors' directNeighborOffsets

diagonalNeighbors :: V2 Int -> [V2 Int]
diagonalNeighbors = neighbors' diagonalNeighborOffsets

neighbors :: V2 Int -> [V2 Int]
neighbors p = directNeighbors p ++ diagonalNeighbors p
