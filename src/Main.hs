
import qualified Prelude
import           Relude
import           Project             (Project, Repository)
import qualified Project             as Pr
import           ProjectTree
import           Options.Applicative
import qualified System.Environment  as Env
import           System.Directory

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
instance Show Action where
    show List                = "List"
    show (Add wiki desc dir) = "Add " <> show desc <> " (" <> show dir <> ") " <> show wiki
    show Remove              = "Remove"
    show (Get _)             = "Get ..."
    show (Set _)             = "Set ..."
    show (Repo ra)           = "Repo " <> show ra
    show Activate            = "Activate"
    show Deactivate          = "Deactivate"

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
main = execOptions =<< mkOptions =<< execParser cliOpt
 where cliOpt :: ParserInfo Options
       cliOpt = info cliOptions ( fullDesc
                               <> progDesc "Manage a set of projects and their properties"
                               <> header "projects -- a project manager"
                                )

execOptions :: Options -> IO ()
execOptions (Options projName action ptroot uall active) = do
    ptree <- readProjectTree $ toString ptroot
    result <- execAction action uall active projName ptree
    writeProjectTree (toString ptroot) result

execAction :: Action -> Bool -> Bool -> Text -> ProjectTree -> IO ProjectTree
execAction List _ act proj ptree = mapM_ putTextLn names >> return ptree 
 where names :: [Text]
       names = snd $ onSubTree (id &&& foldProjectTree getName) ptree proj
       getName :: Project -> [Text]
       getName pr = if not act || Pr.active pr then return $ Pr.name pr
                                               else []

execAction (Add wiki desc mdir) _ _ proj ptree = 
    if hasProject proj ptree
       then putTextLn ("Project " <> proj <> " already exists") >> return ptree
       else do dir <- fromString <$> maybe getCurrentDirectory (return . toString) mdir
               let project = Pr.Project proj desc dir [] wiki False
               return $ addProject project ptree

execAction Remove rc act proj ptree =
    if not rc && not (isSimpleProject proj ptree)
       then if hasProject proj ptree
               then return $ fst $ execOnOneProject (const (Nothing,Nothing)) ptree proj
               else putTextLn ("Won't recursively remove project " <> proj) >> return ptree
       else return $ delProject proj ptree

execAction (Get getter) rc act proj ptree = do
    if not rc && not (isSimpleProject proj ptree)
       then if hasProject proj ptree
               then putTextLn (maybe "" id $ snd $ execOnOneProject (Just &&& Just . getter) ptree proj)
               else putTextLn ("Project " <> proj <> " has subprojects, use -u to get from all of them")
       else mapM_ putTextLn fields
    return ptree
 where actGetter :: Project -> [Text]
       actGetter pr = if not act || Pr.active pr then return $ getter pr
                                                 else []
       fields :: [Text]
       fields = snd $ onSubTree (id &&& foldProjectTree actGetter) ptree proj

execAction (Set setter) rc act proj ptree =
    if not rc && not (isSimpleProject proj ptree)
       then if hasProject proj ptree
               then return $ fst $ execOnOneProject (Just . setter &&& const Nothing) ptree proj
               else putTextLn ("Project " <> proj <> " has subprojects, use -u to set to all of them")
                 >> return ptree
       else return $ fst $ onSubTree (projectMap actSetter &&& const Nothing) ptree proj
 where actSetter :: Project -> Project
       actSetter pr = if not act || Pr.active pr then setter pr else pr

execAction (Repo repoA) rc act proj ptree = execRepoAction repoA rc act proj ptree

execAction Activate rc act proj ptree =
    if not rc && not (isSimpleProject proj ptree)
       then if hasProject proj ptree
               then return $ fst $ execOnOneProject (Just . setter &&& const Nothing) ptree proj
               else putTextLn ("Project " <> proj <> " has subprojects, use -u to activate all of them")
                 >> return ptree
       else return $ fst $ onSubTree (projectMap actSetter &&& const Nothing) ptree proj
 where actSetter :: Project -> Project
       actSetter pr = if not act || Pr.active pr then setter pr else pr
       setter :: Project -> Project
       setter pr = pr { Pr.active = True }

execAction Deactivate rc act proj ptree =
    if not rc && not (isSimpleProject proj ptree)
       then if hasProject proj ptree
               then return $ fst $ execOnOneProject (Just . setter &&& const Nothing) ptree proj
               else putTextLn ("Project " <> proj <> " has subprojects, use -u to deactivate all of them")
                 >> return ptree
       else return $ fst $ onSubTree (projectMap actSetter &&& const Nothing) ptree proj
 where actSetter :: Project -> Project
       actSetter pr = if not act || Pr.active pr then setter pr else pr
       setter :: Project -> Project
       setter pr = pr { Pr.active = False }

execRepoAction :: RepoAction -> Bool -> Bool -> Text -> ProjectTree -> IO ProjectTree
execRepoAction = undefined


--    ___        _   _               ____                          
--   / _ \ _ __ | |_(_) ___  _ __   |  _ \ __ _ _ __ ___  ___ _ __ 
--  | | | | '_ \| __| |/ _ \| '_ \  | |_) / _` | '__/ __|/ _ \ '__|
--  | |_| | |_) | |_| | (_) | | | | |  __/ (_| | |  \__ \  __/ |   
--   \___/| .__/ \__|_|\___/|_| |_| |_|   \__,_|_|  |___/\___|_|   
--        |_|                                                      
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
getOptions = subparser
           $ command "name"        (info (fromTGetter Pr.name)        $ progDesc "Get project name")
          <> command "description" (info (fromTGetter Pr.description) $ progDesc "Get project description")
          <> command "directory"   (info (fromTGetter Pr.directory)   $ progDesc "Get project directory")
          <> command "wiki"        (info (fromTGetter Pr.wiki)        $ progDesc "Get project wiki")
          <> command "activated"   (info (fromGetter  Pr.active)      $ progDesc "Is project active")
 where fromGetter :: Show a => (Project -> a) -> Parser Action
       fromGetter getter = pure $ Get $ show . getter
       fromTGetter :: (Project -> Text) -> Parser Action
       fromTGetter getter = pure $ Get getter

setOptions :: Parser Action
setOptions = subparser
           $ fromSetter "directory" (\dir p -> p { Pr.directory = dir })
          <> fromSetter "wiki"      (\file p -> p { Pr.wiki = file })
          <> command "description" (info descParser $ progDesc "Set project description")
 where fromSetter :: Text -> (Text -> Project -> Project) -> Mod CommandFields Action
       fromSetter name setter = let nm = toString name
                                 in command nm (info (Set . setter <$> argument str (metavar nm))
                                                   $ progDesc $ "Set project " ++ nm)
       descParser :: Parser Action
       descParser = Set . (\desc p -> p { Pr.description = desc }) . mconcat . intersperse " "
                <$> many (argument str (metavar "description"))

reposOptions :: Parser Action
reposOptions = Repo <$> subparser
             ( command "list"   (info reposListOptions $ progDesc "List repositories")
            <> command "remove" (info reposRmOptions   $ progDesc "Remove repository in current directory")
            <> command "add"    (info reposAddOptions  $ progDesc "Add repository in current directory")
             )

reposListOptions :: Parser RepoAction
reposListOptions = pure RList

reposRmOptions :: Parser RepoAction
reposRmOptions = pure RRemove 

reposAddOptions :: Parser RepoAction
reposAddOptions = subparser
                $ command "git"   (info reposGitOptions   $ progDesc "Git repository")
               <> command "other" (info reposOtherOptions $ progDesc "Arbitrary repository")

reposGitOptions :: Parser RepoAction
reposGitOptions = RAddGit <$> argument str (metavar "remote")

reposOtherOptions :: Parser RepoAction
reposOtherOptions = RAddOther 
                <$> mconcat . intersperse " "
                <$> many (argument str (metavar "command"))

actOptions :: Parser Action
actOptions = pure Activate

deactOptions :: Parser Action
deactOptions = pure Deactivate



