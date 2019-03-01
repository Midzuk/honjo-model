module Main where

import qualified System.Directory as Dir

import Lib

main :: IO ()
main = do
  lc <- decodeMesh "input/mesh_honjo.csv"
  nc <- decodeFacility "input/facility_lonlat.csv"

  cd <- Dir.getCurrentDirectory
  -- writeFile (cd <> "/output/path/path.csv") $ encodePath nc p

  print ()
