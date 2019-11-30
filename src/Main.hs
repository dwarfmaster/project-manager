
import qualified Prelude
import           Relude
import           Project             (Project, Repository)
import qualified Project             as Pr
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
            | Add { wiki :: Text, description :: Text, directory :: Maybe Text }
            | Remove
            | Get { getter :: Project -> Text }
            | Set { setter :: Project -> Project }
            | Repo RepoAction
            | Activate
            | Deactivate
            | Help
instance Show Action where
    show List                = "List"
    show (Add wiki desc dir) = "Add " <> show desc <> " (" <> show dir <> ") " <> show wiki
    show Remove              = "Remove"
    show (Get _)             = "Get ..."
    show (Set _)             = "Set ..."
    show (Repo ra)           = "Repo " <> show ra
    show Activate            = "Activate"
    show Deactivate          = "Deactivate"
    show Help                = "Help"

data Options = Options { project   :: Text   -- Might point anywhere on the project tree, the action
                                             -- will be executed on all sub projects (if the action
                                             -- will cause the modification of more than one projects,
                                             -- it will only be executed if updateall is true).
                                             -- The root of the tree is designed by a dot.
                       , act       :: Action -- The action to execute.
                       , ptroot    :: Text   -- The path to the root of the project tree, if unset
                                             -- it will be assumed to be $PROJECT_ROOT,
                                             -- $XDG_CONFIG_HOME/projects or ~/.config/projects, in that
                                             -- order.
                       , updateall :: Bool   -- When an action will cause the updating of more than one
                                             -- project, should we proceed (if true) or warn the user.
                                             -- Defaults to false.
                       , onlyact   :: Bool   -- Only act on/show actives projects
                       } deriving (Show)
mkOptions :: Options -> IO Options
mkOptions opts@(Options _ _ root _ _) = if root /= "" then return opts else do
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
         <*> flag False True
                  ( long "actives"
                 <> short 'a'
                 <> help "Only show/operate on active projects")

parseAction :: Parser Action
parseAction = subparser
            $ command "list"       (info listOptions   $ progDesc "List the projects")
           <> command "add"        (info addOptions    $ progDesc "Add a new project")
           <> command "remove"     (info removeOptions $ progDesc "Remove a project (or subtree)")
           <> command "get"        (info getOptions    $ progDesc "Get a property from a project(s)")
           <> command "set"        (info setOptions    $ progDesc "Set project(s) property")
           <> command "repos"      (info reposOptions  $ progDesc "Access the repos information")
           <> command "activate"   (info actOptions    $ progDesc "Mark project(s) as active")
           <> command "deactivate" (info deactOptions  $ progDesc "Mark project(s) as inactive")
           <> command "help"       (info helpOptions   $ progDesc "Show help")

listOptions :: Parser Action
listOptions = pure List

addOptions :: Parser Action
addOptions = Add
         <$> argument str (metavar "wiki")
         <*> (mconcat . intersperse " " <$> many (argument str (metavar "description")))
         <*> optional (strOption ( long "directory"
                                <> short 'd'
                                <> help "The directory of the project"))

removeOptions :: Parser Action
removeOptions = pure Remove

getOptions :: Parser Action
getOptions = pure $ Get $ const "caca" -- TODO

setOptions :: Parser Action
setOptions = pure $ Set id -- TODO

reposOptions :: Parser Action
reposOptions = pure $ Repo RList -- TODO

actOptions :: Parser Action
actOptions = pure Activate

deactOptions :: Parser Action
deactOptions = pure Deactivate

helpOptions :: Parser Action
helpOptions = pure Help



