module Logic ((&&&))
  where

-- Predicate combination operator
(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) f g = \ input -> f input && g input
