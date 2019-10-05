#! /usr/bin/env nix-shell
#! nix-shell -p "ghc.withPackages (p: with p; [ selective ])"
#! nix-shell -i "runhaskell --ghc-arg=-Wall"
#! nix-shell --pure
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Selective
import Data.Monoid (Sum(..))

main :: IO ()
main = checkFoo

checkFoo :: IO ()
checkFoo = do
  putStr "Enter something: "
  ifS ((== "foo") <$> getLine)
    (putStrLn "thanks for the foo")
    (putStrLn ("that's not a foo!"))
  print $ findS (\_ -> MyUnder (MyConst (Sum (1::Int)))) ['a', 'b', 'c']
  print $ findS (\_ -> MyOver (MyConst (Sum (1::Int)))) ['a', 'b', 'c']

newtype MyConst a whateverlolidontcare = MyConst a deriving Show

newtype MyOver a b = MyOver { getOver :: MyConst a b} deriving (Show, Functor, Applicative)
newtype MyUnder a b = MyUnder { getUnder :: MyConst a b} deriving (Show, Functor, Applicative)

instance Functor (MyConst a) where
  fmap _ (MyConst x) = MyConst x

instance Monoid a => Applicative (MyConst a) where
  pure _ = MyConst mempty
  MyConst f <*> MyConst a = MyConst (f <> a)

instance Monoid a => Selective (MyOver a) where
  select (MyOver (MyConst c)) (MyOver (MyConst t)) = MyOver (MyConst (c <> t))

instance Monoid a => Selective (MyUnder a) where
  select (MyUnder (MyConst c)) _ = MyUnder (MyConst c)

findS :: Selective f => (a -> f Bool) -> [a] -> f (Maybe a)
findS _ [] = pure Nothing
findS p (x:xs) = ifS (p x) (pure (Just x)) (findS p xs)
