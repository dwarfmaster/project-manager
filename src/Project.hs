
module Project (Repository(..), Project(..), readProject, writeProject) where

import           Prelude ()
import           Relude
import           Relude.String.Conversion
import           Relude.File
import           Data.Proxy
import qualified Data.Text.Prettyprint.Doc as Pretty
import           Data.Maybe
import           Dhall
import           Dhall.Core
import           Dhall.TH
import           Dhall.Format
import qualified Dhall.Map                 as Mp

class ToDhall a where
    dhallType :: Proxy a -> Expr Void Void
    toDhall :: a -> Expr Void Void
instance ToDhall Text where
    dhallType _ = Text
    toDhall = TextLit . Chunks []
instance ToDhall a => ToDhall [a] where
    dhallType _ = App List $ dhallType (Proxy @a)
    toDhall [] = ListLit (Just $ dhallType (Proxy @[a])) []
    toDhall ls = ListLit Nothing $ fromList $ fmap toDhall ls

data Injection a = Inj (a -> Expr Void Void) (Expr Void Void)
mkInj :: forall a b. ToDhall b => (a -> b) -> Injection a
mkInj f = Inj (toDhall . f) $ dhallType $ Proxy @b

makeRecordType :: [(Text, Injection a)] -> Expr Void Void
makeRecordType = Record . fromList . fmap (\(name,Inj _ tp) -> (name,tp))
convertRecordType :: [(Text, Injection a)] -> a -> Expr Void Void
convertRecordType fields record = RecordLit $ fromList $ fmap (\(name,Inj i _) -> (name, i record)) fields
convertUnionType :: Mp.Map Text [(Text, Injection a)] -> Text -> a -> Expr Void Void
convertUnionType fields field union =
    UnionLit field
             (convertRecordType (fromMaybe [] $ Mp.lookup field fields) union)
             (makeRecordType <$> Mp.delete field fields)

data Repository = Git { url :: Text, user :: Text, name :: Text }
                | Other { command :: Text }
                deriving (Generic,Show)
injectRepository :: Mp.Map Text [(Text, Injection Repository)]
injectRepository = Mp.fromList
                 [ ("Git", [ ("url",  mkInj url)
                           , ("user", mkInj user)
                           , ("name", mkInj (name :: Repository -> Text))
                           ])
                 , ("Other", [ ("other", mkInj command) ])
                 ]
instance Interpret Repository
instance ToDhall Repository where
    dhallType _ = Union $ fmap makeRecordType injectRepository
    toDhall git@Git{}   = convertUnionType injectRepository "Git"   git
    toDhall oth@Other{} = convertUnionType injectRepository "Other" oth

data Project = Project
             { name        :: Text
             , description :: Text
             , directory   :: Text
             , repos       :: [Repository]
             , wiki        :: Text
             } deriving (Generic,Show)
injectProject :: [(Text, Injection Project)]
injectProject = [ ("name",        mkInj (name :: Project -> Text))
                , ("description", mkInj description)
                , ("directory",   mkInj (directory :: Project -> Text))
                , ("repos",       mkInj repos)
                , ("wiki",        mkInj wiki)
                ]
instance Interpret Project
instance ToDhall Project where
    dhallType _ = Record $ fromList $ fmap (\(name,Inj _ tp) -> (name,tp)) injectProject 
    toDhall = convertRecordType injectProject

readProject :: FilePath -> IO (Maybe Project)
readProject = readFileText >=> input auto

writeProject :: FilePath -> Project -> IO ()
writeProject path = (return . pretty . toDhall) >=> writeFileText path

