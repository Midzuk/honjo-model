module Main where

import Lib

main :: IO ()
main = do
  lc <- decodeLinkCsv "/output/simple_links.csv"
  nc <- decodeNodeCsv "/output/simple_nodes.csv"
  
  let (p, dorg, ddest) = shortestPathCSV od nc lc

  cd <- Dir.getCurrentDirectory
  writeFile (cd <> "/output/path/path.csv") $ encodePath nc p

  print (costLink $ cost p, dorg, ddest)
