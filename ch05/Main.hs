module Main where

import SimpleJson
import PutJson

main :: IO ()
main = putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])