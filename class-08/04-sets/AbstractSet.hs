module AbstractSet where

class AbstractSet a where
  empty :: a t
  push :: Ord t => a t -> t -> a t
  contains :: Ord t => a t -> t -> Bool
  remove :: Ord t => a t -> t -> a t
  toList :: Ord t => a t -> [t]
  fromList :: Ord t => [t] -> a t