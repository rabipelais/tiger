module Main where

import           ExpToDOT              (expToDOT)
import           Parser                (parser)
import           Protolude
import           Text.Megaparsec.Error (parseErrorPretty)

main :: IO ()
main = do
  args <- getArgs
  sources <- mapM readFile args
  let zipped = zip args sources
  putText "====== SOURCE ======="
  mapM_ (\(t, f) -> putStrLn t >> putStrLn f) zipped
  putText "====== PARSE ======="
  let results' = map (\(t, f) -> parser (toS t) (toS f)) zipped
  let results = zip args results'
  mapM_ (\(t, r) -> either
          (\a -> hPutStrLn stderr (parseErrorPretty a))
          (\x -> (putStrLn t) >> (putStrLn $ expToDOT x)) r)
    results
