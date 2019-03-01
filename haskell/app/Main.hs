module Main where

import qualified System.Directory as Dir

import           Lib

main :: IO ()
main = do
  ms <- decodeMesh "/input/mesh_honjo.csv"
  fs <- decodeFacility "/input/facility_lonlat.csv"

  let xs = distance <$> ms <*> fs

  cd <- Dir.getCurrentDirectory
  writeFile (cd <> "/output/distance.csv") $ encodeDist xs
