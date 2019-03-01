module Main where

import Lib

main :: IO ()
main = do
  lc <- decodeMesh "mesh_honjo.csv"
  nc <- decodeNodeCsv "facility_lonlat.csv"

  cd <- Dir.getCurrentDirectory
  -- writeFile (cd <> "/output/path/path.csv") $ encodePath nc p

  print ()
