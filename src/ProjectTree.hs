
module ProjectTree where

import qualified Prelude
import           Relude
import           Project
import           System.Directory
import           System.FilePath.Posix
import qualified Data.Map              as Mp
import qualified Data.Text             as T

data ProjectTree = PTree
                 { projects :: Map Text Project
                 , children :: Map Text ProjectTree
                 } deriving (Show)

foldProjectTree :: forall m. Monoid m => (Project -> m) -> ProjectTree -> m
foldProjectTree = foldProjectTreeRec
 where foldProjectTreeRec :: (Project -> m) -> ProjectTree -> m 
       foldProjectTreeRec f (PTree projs childs) =
           foldMap f projs <> foldMap (foldProjectTreeRec f) childs

readProjectTree :: FilePath -> IO ProjectTree
readProjectTree path = do
    createDirectoryIfMissing True path
    withCurrentDirectory path $ readProjectTreeRec "./root"

readProjectTreeRec :: FilePath -> IO ProjectTree
readProjectTreeRec path = do
    let fullpath = path <> "/"
    contents <- listDirectory path
    children <- filterM (doesDirectoryExist . (fullpath <>)) contents
    projects <- filterM (doesFileExist      . (fullpath <>)) contents
    PTree <$> (fromList <$> mapM (prepProj  fullpath) projects)
          <*> (fromList <$> mapM (prepChild fullpath) children)
 where prepProj :: FilePath -> FilePath -> IO (Text,Project)
       prepProj root path = (fromString $ dropExtension path,)
                        <$> readProject (root <> path)
       prepChild :: FilePath -> FilePath -> IO (Text,ProjectTree)
       prepChild root path = (fromString path,) <$> readProjectTreeRec (root <> path)

writeProjectTree :: FilePath -> ProjectTree -> IO ()
writeProjectTree path ptree = do
    removeDirectory path
    let fullpath = path <> "/root"
    createDirectoryIfMissing True fullpath
    withCurrentDirectory fullpath $ writeProjectTreeRec ptree

writeProjectTreeRec :: ProjectTree -> IO ()
writeProjectTreeRec (PTree projs childs) = do
    let projlst = Mp.toList projs
    let childlst = Mp.toList childs
    forM_ projlst $ \(k,proj) -> writeProject (toString k <> ".dhall") proj
    forM_ childlst $ \(k,child) -> writeProjectTreeRec' (toString k) child

writeProjectTreeRec' :: FilePath -> ProjectTree -> IO ()
writeProjectTreeRec' path ptree = do
    createDirectoryIfMissing True path
    withCurrentDirectory path $ writeProjectTreeRec ptree

listProjectsName :: ProjectTree -> [Text]
listProjectsName = foldProjectTree $ return . (name :: Project -> Text)

splitProjectName :: Text -> [Text]
splitProjectName = T.splitOn "."

onBranch :: forall f a. Alternative f => (ProjectTree -> f a) -> ProjectTree -> Text -> f a
onBranch action ptree name = onBranch' (splitProjectName name) ptree
 where onBranch' :: [Text] -> ProjectTree -> f a
       onBranch' []       pt               = action pt
       onBranch' (nm:nms) (PTree _ childs) = maybe empty (onBranch' nms) $ Mp.lookup nm childs

onProject :: forall f a. Alternative f => (Project -> f a) -> ProjectTree -> Text -> f a
onProject action ptree name = onProject' (splitProjectName name) ptree
 where onProject' :: [Text] -> ProjectTree -> f a
       onProject' []     (PTree projs childs) = empty
       onProject' [p]    (PTree projs childs) = maybe empty action $ Mp.lookup p projs
       onProject' (p:ps) (PTree projs childs) = maybe empty (onProject' ps) $ Mp.lookup p childs

hasProject :: ProjectTree -> Text -> Bool
hasProject ptree name = maybe False id $ onProject (const $ Just True) ptree name

projectMap :: (Project -> Project) -> ProjectTree -> ProjectTree
projectMap action (PTree projs childs) = PTree (fmap action projs) $ fmap (projectMap action) childs

addProject :: Project -> ProjectTree -> ProjectTree
addProject pr = addProject' (splitProjectName $ (name :: Project -> Text) pr) pr
 where addProject' :: [Text] -> Project -> ProjectTree -> ProjectTree
       addProject' []         _  _                    = error "Trying to add a project without a name"
       addProject' [name]     pr (PTree projs childs) = PTree (Mp.insert name pr projs) childs
       addProject' (name:nms) pr (PTree projs childs) =
           let pt = Mp.findWithDefault (PTree Mp.empty Mp.empty) name childs
            in PTree projs $ Mp.insert name (addProject' nms pr pt) childs

-- It wont recursively delete a subtree
delProject :: Text -> ProjectTree -> ProjectTree
delProject name = delProject' $ splitProjectName name
 where delProject' :: [Text] -> ProjectTree -> ProjectTree
       delProject' []         pt = pt
       delProject' [name]     (PTree projs childs) = PTree (Mp.delete name projs) childs
       delProject' (name:nms) (PTree projs childs) =
           case Mp.lookup name childs of
             Nothing -> PTree projs childs
             Just pt -> let npt = delProject' nms pt
                         in if Mp.null (projects npt) && Mp.null (children npt)
                               then PTree projs $ Mp.delete name childs
                               else PTree projs $ Mp.insert name npt childs

