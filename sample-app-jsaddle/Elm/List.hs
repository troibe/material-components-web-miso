module Elm.List
  ( indexedMap,
  )
where

indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f xs =
  let indexedMapHelper _ [] _ = []
      indexedMapHelper f (x : xs) i = f i x : indexedMapHelper f xs (i + 1)
   in indexedMapHelper f xs 0
