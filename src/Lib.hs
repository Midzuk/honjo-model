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
data Mesh = Mesh MeshCode Longitude Latitude deriving (Show)

instance FromNamedRecord LinkCsvOut where
  parseNamedRecord m =
    LinkCsvOut
      <$> m .: "node_id_org"
      <*> m .: "node_id_dest"
      <*> m .: "distance"



type LinkCsv = V.Vector LinkCsvOut

decodeLinkCsv :: FilePath -> IO LinkCsv
decodeLinkCsv fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, LinkCsv)
  return ls



data NodeCsvOut = NodeCsvOut NodeId Latitude Longitude deriving Show

instance FromNamedRecord NodeCsvOut where
  parseNamedRecord m =
    NodeCsvOut
      <$> m .: "node_id"
      <*> m .: "latitude"
      <*> m .: "longitude"

decodeNodeCsv :: FilePath -> IO NodeCsv
decodeNodeCsv fp = do
  cd <- Dir.getCurrentDirectory
  bs <- B.readFile (cd <> fp)
  let Right (_, ls) = decodeByName bs :: Either String (Header, V.Vector NodeCsvOut)
  return $ makeNodeCsv ls



type NodeCsv =
  Map.Map NodeId Coordinates

makeNodeCsv :: V.Vector NodeCsvOut -> NodeCsv
makeNodeCsv = foldr f Map.empty
  where
    f (NodeCsvOut ni lat lon) = Map.insert ni $ Coordinates lat lon



makeLinks :: NodeCsv -> LinkCsv -> V.Vector Link
makeLinks nc = foldr f []
  where
    f (LinkCsvOut org dest dist) = V.cons (org :->: Node dest c $ dist)  
      where
        c = nc Map.! dest

nearestNode :: Coordinates -> NodeCsv -> LinkCsv -> Node --Bool -> Coordinates -> NodeCsv -> LinkCsv -> NodeId
nearestNode c (Map.assocs -> ncs) lc = Node { nodeId = ni, coordinates = c1 } -- ncs1でなくてもOK?
  where
    (ni, c1) = minimumBy (compare `on` (snd . fmap (grtCirDist c))) ncs
    {-
    f (LinkCsvOut org dest _) =
      if True -- b
        then org
        else dest
        
    ncs1 = filter (\(n, _) -> n `V.elem` (f <$> lc)) ncs
    -}

makeLink :: OD -> NodeCsv -> LinkCsv -> (Link, Distance, Distance)
makeLink (OD c1 c2) nc lc = ( nodeId org :->: dest $ dist, distOrg, distDest)
  where
    org = nearestNode c1 nc lc
    dest = nearestNode c2 nc lc
    dist = grtCirDist (coordinates org) (coordinates dest)
    distOrg = grtCirDist c1 (coordinates org)
    distDest = grtCirDist c2 (coordinates dest)

shortestPathCSV :: OD -> NodeCsv -> LinkCsv -> (Path, Distance, Distance)
shortestPathCSV od nc lc = (shortestPath l (makeLinks nc lc), dorg, ddest)
  where
    (l, dorg, ddest) = makeLink od nc lc :: (Link, Distance, Distance)
    

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