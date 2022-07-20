{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Main where

import Control.Monad.Except
import Nock
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = do
  mapM_
    (\(b, f, toNoun) -> quickCheck (formula_correctness b f . toNoun))
    formula_specs

formula_specs :: [(Noun, Noun -> Noun, Int -> Noun)]
formula_specs =
  [ (inc, \(Atom a) -> Atom (a + 1), Atom),
    (dec, \(Atom a) -> Atom (a + 1), Atom . (+ 1) . abs)
  ]

formula_correctness :: Noun -> (Noun -> Noun) -> Noun -> Property
formula_correctness b f a =
  monadicIO . run $
    runExceptT (applyM b a) >>= \case
      Left err -> pure False
      Right c -> pure $ c == f a