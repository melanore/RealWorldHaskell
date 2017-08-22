import System.Environment (getArgs)

process :: (String -> String) -> FilePath -> FilePath -> IO ()
process f input output = do
    input <- readFile input
    writeFile output (f input)

splitlines :: [Char] -> [[Char]]
splitlines cs =
    let (pre, suf) = break isLineTerminator cs
    in pre : case suf of
        ('\r':'\n':rest)    -> splitlines rest
        ('\r':rest)         -> splitlines rest
        ('\n':rest)         -> splitlines rest
        _                   -> []
    
isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'

main :: IO ()
main = do  
    args <- getArgs
    case args of
        [input, output] -> process f input output where f = unlines . splitlines
        _               -> putStrLn "error: exactly two arguments needed"
