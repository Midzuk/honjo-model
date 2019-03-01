{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.ByteString.Lazy as B
import           Data.Csv             (FromNamedRecord (..), Header,
                                        decodeByName, (.:))
import qualified Data.Vector as V
import qualified System.Directory as Dir

-- import qualified Data.Map.Lazy        as Map
-- import           Data.Maybe           (isJust)
-- import qualified Data.Text            as T
-- import qualified Data.Set             as Set
-- import           Data.Foldable        (minimumBy)
-- import           Data.Function        (on) 


type MeshCode = Int
type Longitude = Double
type Latitude = Double
data Mesh = Mesh MeshCode Longitude Latitude deriving Show

instance FromNamedRecord Mesh where
  parseNamedRecord m =
    Mesh
      <$> m .: "mesh_code"
      <*> m .: "longitude"
      <*> m .: "latitude"

decodeMesh :: FilePath -> IO (V.Vector Mesh)
decodeMesh fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ms) = decodeByName bs :: Either String (Header, V.Vector Mesh)
  return ms


type Type = B.ByteString
data Facility = Facility Type Int Longitude Latitude deriving Show

instance FromNamedRecord Facility where
  parseNamedRecord m =
    Facility
      <$> m .: "type"
      <*> m .: "num"
      <*> m .: "longitude"
      <*> m .: "latitude"

decodeFacility :: FilePath -> IO (V.Vector Facility)
decodeFacility fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, fs) = decodeByName bs :: Either String (Header, V.Vector Facility)
  return fs



distance :: Mesh -> Facility -> (MeshCode, Type, Int, Double)
distance (Mesh mc lon1 lat1) (Facility t n lon2 lat2) =
  (mc, t, n, grtCirDist (lon1, lat1) (lon2, lat2))


grtCirDist :: (Longitude, Latitude) -> (Longitude, Latitude) -> Double
grtCirDist (lon1, lat1) (lon2, lat2) =
  6378137 * acos (sin (f lat1) * sin (f lat2) + cos (f lat1) * cos (f lat2) * cos (f lon1 - f lon2))
  where
    f = ((pi / 180) *)

encodeDist :: V.Vector (MeshCode, Type, Int, Double) -> String
encodeDist xs =
  "mesh_code,type,num,distance"
    <> foldr
      (\ (mc, t, n, d) str ->
          str
            <> "\n"
            <> show mc <> ","
            <> show t <> ","
            <> show n <> ","
            <> show d
      )
      "" xs