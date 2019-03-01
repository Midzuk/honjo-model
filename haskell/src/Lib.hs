module Lib
    ( someFunc
    ) where

import qualified Data.ByteString.Lazy as B
import           Data.Csv             (FromNamedRecord (..), Header,
                                        decodeByName, (.:))
-- import qualified Data.Map.Lazy        as Map
-- import           Data.Maybe           (isJust)
-- import qualified Data.Text            as T
-- import qualified Data.Vector          as V
-- import qualified System.Directory     as Dir
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
type Num = Int
data Facility = Facility Type Num Longitude Latitude deriving Show

instance FromNamedRecord Facility where
  parseNamedRecord m =
    NodeCsvOut
      <$> m .: "type"
      <*> m .: "num"
      <*> m .: "mesh_code"
      <*> m .: "longitude"
      <*> m .: "latitude"

decodeFacility :: FilePath -> IO (V.Vector Facility)
decodeFacility fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, fs) = decodeByName bs :: Either String (Header, V.Vector Facility)
  return fs




    

encodePath :: NodeCsv -> Path -> String
encodePath nc (graph -> g) =
  "node_id,latitude,longitude"
    <> foldr
      (\ ni str ->
          str
            <> "\n"
            <> show ni <> ","
            <> show (latitude $ nc Map.! ni) <> ","
            <> show (longitude $ nc Map.! ni))
      "" g