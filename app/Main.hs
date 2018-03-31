module Main where

import           ExpToDOT              (expToDOT)
import           Parser                (parser)
import           Protolude
import           Text.Megaparsec.Error (parseErrorPretty)

main :: IO ()
main = do
  args <- getArgs
  mapM_ putStrLn args
  result <- case args of
    []  -> fmap (parser "<stdin>") getContents
    [f] -> parser (toS f) <$> (toS <$> readFile f)
    _   -> undefined
  either (\a -> hPutStrLn stderr (parseErrorPretty a)) (putStrLn . expToDOT) result
