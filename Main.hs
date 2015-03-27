{-# LANGUAGE OverloadedStrings #-}


import Text.PrettyPrint.Boxes
import qualified Data.Text as T
import Data.Aeson (decode, Value(..), FromJSON(..), (.:))
import Network.Wreq
import Control.Lens
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Control.Monad(mzero)


opts = defaults & header "Accept" .~ ["application/json"]

getCircleBuildCollection :: IO (Maybe BuildCollection)
getCircleBuildCollection = do 
  response <- getWith opts "https://circleci.com/api/v1/project/zephyr-dev/gust?circle-token=a5422c509e6c049514733030174a901e8cd17b3e"
  return . decodeBuildCollection $ (response ^. responseBody)

data Build = Build { buildId :: Integer , status :: T.Text , branch :: T.Text} deriving (Show)

instance FromJSON Build where
  parseJSON (Object v) = Build <$> v .: "build_num" <*> v .: "outcome" <*> v .: "branch"
  parseJSON _  = mzero

data BuildCollection = BuildCollection { builds :: [Build] } deriving(Show)
instance FromJSON BuildCollection where
  parseJSON circleBuildCollection@(Array v) = do
    allBuildCollection <- parseJSON circleBuildCollection
    BuildCollection <$> mapM parseJSON allBuildCollection
  parseJSON _ =  mzero

decodeBuildCollection :: BL.ByteString -> Maybe BuildCollection
decodeBuildCollection = decode

headerNames :: [String]
headerNames = [ "Pair", "Branch", "Status", "Time" ]

headerBox :: Box
headerBox  = foldr (<+>) (text "") headerBoxes
  where
    headerBoxes :: [Box]
    headerBoxes = map text headerNames 

buildBox :: Build -> Box
buildBox build = buildBranch <+> buildStatus <+> time --  <+> authors
  where
    {- authors :: Box -}
    {- authors  = text . T.unpack $ authorName build -}
    buildBranch :: Box
    buildBranch = text . T.unpack $ branch build
    buildStatus :: Box
    buildStatus  = text . T.unpack $ status build
    time :: Box
    time  = text "22:19"

buildBoxes :: Maybe BuildCollection -> [Box]
buildBoxes bc = case bc of
                     Nothing -> []
                     Just blds -> map buildBox $ builds blds

main = do 
  buildCollection <- getCircleBuildCollection 
  printBox $ headerBox // (foldr (//) (text "") $ buildBoxes buildCollection )
