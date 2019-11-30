
import qualified Prelude
import           Relude
import           Project
import           ProjectTree
import           Options.Applicative
import qualified System.Environment  as Env

-- A repo action is assumed to be executed at the root of the repo
data RepoAction = RAddGit Text   -- the remote
                | RAddOther Text -- the command
                | RRemove
                | RList
                deriving (Show)

data Action = List
            | Add { description :: Text, directory :: Maybe Text, wiki :: Text }
            | Remove
            | Get { getter :: Project -> Text }
            | Set { setter :: Project -> Project }
            | Repo RepoAction
instance Show Action where
    show List                = "List"
    show (Add desc dir wiki) = "Add " <> show desc <> " (" <> show dir <> ") " <> show wiki
    show Remove              = "Remove"
    show (Get _)             = "Get ..."
    show (Set _)             = "Set ..."
    show (Repo ra)           = "Repo " <> show ra

data Options = Options { project   :: Text   -- Might point anywhere on the project tree, the action
                                             -- will be executed on all sub projects (if the action
                                             -- will cause the modification of more than one projects,
                                             -- it will only be executed if updateall is true).
                                             -- The root of the tree is designed by a dot.
                       , action    :: Action -- The action to execute.
                       , ptroot    :: Text   -- The path to the root of the project tree, if unset
                                             -- it will be assumed to be $PROJECT_ROOT,
                                             -- $XDG_CONFIG_HOME/projects or ~/.config/projects, in that
                                             -- order.
                       , updateall :: Bool   -- When an action will cause the updating of more than one
                                             -- project, should we proceed (if true) or warn the user.
                                             -- Defaults to false.
                       } deriving (Show)
mkOptions :: Options -> IO Options
mkOptions opts@(Options _ _ root _) = if root /= "" then return opts else do
    proot   <- Env.lookupEnv "PROJECT_ROOT"
    xdghome <- Env.lookupEnv "XDG_CONFIG_HOME"
    home    <- Env.getEnv "HOME"
    let nroot = case proot of
                  Just prootv -> prootv
                  Nothing -> case xdghome of
                    Just xdghomev -> xdghomev <> "/projects"
                    Nothing       -> home <> "/.config/projects"
    return $ opts { ptroot = fromString nroot }

main :: IO ()
main = putStrLn . show =<< mkOptions =<< execParser cliOpt
 where cliOpt :: ParserInfo Options
       cliOpt = info cliOptions ( fullDesc
                               <> progDesc "Manage a set of projects and their properties"
                               <> header "projects -- a project manager"
                                )

cliOptions :: Parser Options
cliOptions = Options
         <$> argument str (metavar "project")
         <*> parseAction
         <*> strOption ( long "root"
                      <> short 'r'
                      <> value ""
                      <> help "The root of the project tree")
         <*> flag False True
                  ( long "updateAll"
                 <> short 'u'
                 <> help "Execute modifying action on more than one project")

parseAction :: Parser Action
parseAction = pure List

