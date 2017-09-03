module PutJson where

import Data.List
import SimpleJson

renderJValue :: JValue -> String
renderJValue (JString s)    = show s
renderJValue (JNumber n)    = show n
renderJValue (JBool True)   = "true"
renderJValue (JBool False)  = "false"
renderJValue JNull          = "null"
renderJValue (JObject o)    = "{" ++ pairs o ++ "}"
    where   pairs [] = ""
            pairs ps = intercalate ", " $ map renderPair ps
            renderPair (k,v)    = show k ++ ": " ++ renderJValue v

putJValue :: JValue -> IO ()
putJValue = putStrLn . renderJValue