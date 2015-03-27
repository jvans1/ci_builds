{-# LANGUAGE OverloadedStrings #-}


import Text.PrettyPrint.Boxes
import Data.Char(isSpace)
import System.Process(readProcessWithExitCode)
import qualified Data.Text as T
import Data.Aeson (decode, Value(..), FromJSON(..), (.:))
import Network.Wreq
import Control.Lens
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Control.Monad(mzero)


rstrip = reverse . dropWhile isSpace . reverse
opts = defaults & header "Accept" .~ ["application/json"]

getCircleBuildCollection :: IO (Maybe BuildCollection)
getCircleBuildCollection = do 
  response <- getWith opts "https://circleci.com/api/v1/project/zephyr-dev/gust?circle-token=a5422c509e6c049514733030174a901e8cd17b3e"
  return . decodeBuildCollection $ (response ^. responseBody)

data Build = Build { buildId :: Integer , status :: T.Text , branch :: T.Text, buildAuthor :: T.Text } deriving (Show)

instance FromJSON Build where
  parseJSON (Object v) = Build <$> v .: "build_num" <*> v .: "status" <*> v .: "branch" <*> v.: "author_name"
  parseJSON _  = mzero

data BuildCollection = BuildCollection { builds :: [Build] } deriving(Show)
instance FromJSON BuildCollection where
  parseJSON circleBuildCollection@(Array v) = do
    allBuildCollection <- parseJSON circleBuildCollection
    BuildCollection <$> mapM parseJSON allBuildCollection
  parseJSON _ =  mzero

decodeBuildCollection :: BL.ByteString -> Maybe BuildCollection
decodeBuildCollection = decode

buildBox :: Build -> Box
buildBox build = buildStatus <+> buildBranch
  where
    buildBranch = text . T.unpack $ branch build
    buildStatus :: Box
    buildStatus  = text . T.unpack $ status build

buildBoxesForAuthor ::  Author -> Maybe BuildCollection -> [Box]
buildBoxesForAuthor  author bc = case bc of
                     Nothing -> []
                     Just blds -> map buildBox $ buildsForAuthor blds
                       where
                         buildsForAuthor :: BuildCollection -> [Build]
                         buildsForAuthor bldCollection = filter forAuthor $ builds bldCollection 
                           where 
                             forAuthor :: Build -> Bool
                             forAuthor build = buildAuthor build == T.pack author

type Author = String

currentAuthors :: IO Author
currentAuthors = do
  (_, authors, _) <- readProcessWithExitCode "git" ["config", "user.name"] ""
  return $ rstrip authors

main = do 
  buildCollection <- getCircleBuildCollection 
  authors <- currentAuthors
  printBox $ foldr (//) (text "") $ buildBoxesForAuthor authors buildCollection
