
module Project (Remote(..), Repository(..), Project(..), readProject, writeProject) where

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
    toDhall [] = ListLit (Just $ dhallType (Proxy @a)) []
    toDhall ls = ListLit Nothing $ fromList $ fmap toDhall ls
instance ToDhall Bool where
    dhallType _ = Bool
    toDhall = BoolLit

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

data Remote = Git { url :: Text }
            | Other { command :: Text }
            deriving (Generic,Show)
injectRemote :: Mp.Map Text [(Text, Injection Remote)]
injectRemote = Mp.fromList
             [ ("Git", [ ("url",  mkInj url) ])
             , ("Other", [ ("command", mkInj command) ])
             ]
instance Interpret Remote
instance ToDhall Remote where
    dhallType _ = Union $ fmap makeRecordType injectRemote
    toDhall git@Git{}   = convertUnionType injectRemote "Git"   git
    toDhall oth@Other{} = convertUnionType injectRemote "Other" oth

data Repository = Repo { directory :: Text, remote :: Remote }
                deriving (Generic,Show)
injectRepository :: [(Text, Injection Repository)]
injectRepository = [ ("directory", mkInj (directory :: Repository -> Text))
                   , ("remote",    mkInj remote)
                   ]
instance Interpret Repository
instance ToDhall Repository where
    dhallType _ = Record $ fromList $ fmap (\(name,Inj _ tp) -> (name,tp)) injectRepository
    toDhall = convertRecordType injectRepository 

data Project = Project
             { name        :: Text
             , description :: Text
             , directory   :: Text
             , repos       :: [Repository]
             , wiki        :: Text
             , active      :: Bool
             } deriving (Generic,Show)
injectProject :: [(Text, Injection Project)]
injectProject = [ ("name",        mkInj (name :: Project -> Text))
                , ("description", mkInj description)
                , ("directory",   mkInj (directory :: Project -> Text))
                , ("repos",       mkInj repos)
                , ("wiki",        mkInj wiki)
                , ("active",      mkInj active)
                ]
instance Interpret Project
instance ToDhall Project where
    dhallType _ = Record $ fromList $ fmap (\(name,Inj _ tp) -> (name,tp)) injectProject 
    toDhall = convertRecordType injectProject

readProject :: FilePath -> IO Project
readProject = readFileText >=> input auto

writeProject :: FilePath -> Project -> IO ()
writeProject path = (return . pretty . toDhall) >=> writeFileText path

