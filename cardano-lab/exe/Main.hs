{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Cardano.Prelude

import Lab (program, programIO)

main :: IO ()
main = do
  _ <- programIO program
  pure ()
