module Madness where

import Data.Monoid

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

-- (<>) or mappend is (++) here
madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

-- rewritten above using mconcat
madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat [e, "! he said ", adv, " as he jumped into his car ", noun,
           " and drove off with his ", adj, " wife."]