
module ProjectTree where

import qualified Prelude
import           Relude
import           Project
import           System.Directory
import           System.FilePath.Posix
import qualified Data.Map              as Mp
import qualified Data.Text             as T
import qualified Data.Set              as St

data ProjectTree = PTree (Maybe Project)        -- The root project
                         (Map Text ProjectTree) -- The subprojects
                         deriving (Show)
instance Semigroup ProjectTree where
    (PTree p1 c1) <> (PTree p2 c2) = PTree (p1 <|> p2) $ Mp.unionWith (<>) c1 c2
instance Monoid ProjectTree where
    mempty = emptyTree 

emptyTree :: ProjectTree
emptyTree = PTree Nothing Mp.empty

readProjectTree :: FilePath -> IO ProjectTree
readProjectTree path = do
    createDirectoryIfMissing True path
    withCurrentDirectory path $ readProjectTreeRoot "./root"

readProjectTreeRoot :: FilePath -> IO ProjectTree
readProjectTreeRoot path = do
    let dhallFile = path <> ".dhall"
    project <- ifM (not <$> doesFileExist dhallFile) (return Nothing) $ Just <$> readProject dhallFile
    childs  <- readProjectTreeChildren path
    return $ PTree project childs

readProjectTreeChildren :: FilePath -> IO (Map Text ProjectTree)
readProjectTreeChildren path =
    let fullpath = path <> "/"
     in doesDirectoryExist fullpath >>= \exists ->
         if not exists
           then return Mp.empty
           else do
               let fullpath = path <> "/"
               content  <- listDirectory path
               children <-                       filterM (doesDirectoryExist . (fullpath <>)) content
               projects <- map dropExtension <$> filterM (doesFileExist      . (fullpath <>)) content
               let set = St.union (fromList children) (fromList projects)
               let all = St.toList set
               childs <- forM all $ \fl -> (fromString fl,) <$> readProjectTreeRoot (fullpath <> fl)
               return $ Mp.fromList childs

writeProjectTree :: FilePath -> ProjectTree -> IO ()
writeProjectTree path ptree = do
    removeDirectoryRecursive path
    let fullpath = path <> "/root"
    createDirectoryIfMissing True fullpath
    withCurrentDirectory path $ writeProjectTreeRec "root" ptree

writeProjectTreeRec :: Text -> ProjectTree -> IO ()
writeProjectTreeRec name (PTree proj childs) = do
    maybe (return ()) (writeProject (toString name <> ".dhall")) proj
    if Mp.null childs
       then return ()
       else do createDirectoryIfMissing True (toString name)
               let childlst = Mp.toList childs
               forM_ childlst $ \(subname',child) ->
                   let subname = toString subname'
                    in withCurrentDirectory (toString name) $ writeProjectTreeRec subname' child

foldProjectTree :: forall m. Monoid m => (Project -> m) -> ProjectTree -> m
foldProjectTree f (PTree proj childs) = maybe mempty f proj <> foldMap (foldProjectTree f) childs

splitProjectName :: Text -> [Text]
splitProjectName txt = if txt == "." then [] else T.splitOn "." txt

nullProject :: ProjectTree -> Bool
nullProject (PTree (Just _) _)      = False
nullProject (PTree Nothing  childs) = all nullProject childs

onSubTree :: forall f a. Alternative f
          => (ProjectTree -> (ProjectTree,f a))
          -> ProjectTree -> Text
          -> (ProjectTree,f a)
onSubTree action ptree name = onSubTreeRec ptree $ splitProjectName name
 where onSubTreeRec :: ProjectTree -> [Text] -> (ProjectTree, f a)
       onSubTreeRec pt                  []       = action pt
       onSubTreeRec pt@(PTree p childs) (nm:nms) =
           case Mp.lookup nm childs of
             Nothing  -> (pt,empty)
             Just spt -> let (npt,result) = onSubTreeRec spt nms
                          in if nullProject npt
                                then (PTree p $ Mp.delete nm     childs, result)
                                else (PTree p $ Mp.insert nm npt childs, result)

projectMap :: (Project -> Project) -> ProjectTree -> ProjectTree
projectMap action (PTree proj childs) =
    PTree (action <$> proj)
        $ Mp.map (projectMap action) childs

-- It will replace an evenrtually existing project
addProject :: Project -> ProjectTree -> ProjectTree
addProject pr = addProject' (splitProjectName $ (name :: Project -> Text) pr) pr
 where addProject' :: [Text] -> Project -> ProjectTree -> ProjectTree
       addProject' []         pr (PTree _    childs) = PTree (Just pr) childs
       addProject' (name:nms) pr (PTree proj childs) =
           let pt = Mp.findWithDefault emptyTree name childs
            in PTree proj $ Mp.insert name (addProject' nms pr pt) childs

-- It will recursively delete a subtree
delProject :: Text -> ProjectTree -> ProjectTree
delProject name ptree = fst $ onSubTree (const (emptyTree,Just ())) ptree name

predProjectTree :: (ProjectTree -> Bool) -> Text -> ProjectTree -> Bool
predProjectTree pred name ptree = maybe False id $ snd
                                $ onSubTree (id &&& return . pred) ptree name

hasProject :: Text -> ProjectTree -> Bool
hasProject = predProjectTree $ \(PTree proj _) -> isJust proj

isSimpleProject :: Text -> ProjectTree -> Bool
isSimpleProject = predProjectTree $ \(PTree _ childs) -> Mp.null childs

execOnOneProject :: forall f a. Alternative f
                 => (Project -> (Maybe Project,f a))
                 -> ProjectTree -> Text
                 -> (ProjectTree, f a)
execOnOneProject action = onSubTree action'
 where action' :: ProjectTree -> (ProjectTree, f a)
       action' (PTree Nothing     childs) = (PTree Nothing childs, empty)
       action' (PTree (Just proj) childs) = let (nproj,result) = action proj
                                             in (PTree nproj childs, result)

