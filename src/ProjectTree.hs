
module ProjectTree where

import qualified Prelude
import           Relude
import           Project
import           System.Directory
import           System.FilePath.Posix

data ProjectTree = PTree
                 { projects :: Map Text Project
                 , children :: Map Text ProjectTree
                 } deriving (Show)

readProjectTree :: FilePath -> IO ProjectTree
readProjectTree path = createDirectoryIfMissing True path >> readProjectTreeRec path

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

