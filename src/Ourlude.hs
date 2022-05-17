module Ourlude (module Prelude, (|>)) where

import Data.Bifunctor (first, second)

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x -- 1 |> \x -> f 0 x 0
{-# INLINE (|>) #-}

infixr 0 <|
(<|) :: (a -> b) -> a -> b
(<|) = ($)
{-# INLINE (<|) #-}