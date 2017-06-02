{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe
import Control.Exception
import qualified GHC.Word
import Data.Time.Format
import Data.Time
import System.Environment (withArgs)
import Control.Concurrent (threadDelay)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Text as OptT
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath
import Test.WebDriver
import Test.WebDriver.Session
import Control.Monad.IO.Class
import qualified Database.SQLite.Simple as SQLite
import Control.Lens
import Control.Logging
import TextShow
import Data.Bool
import Data.Char
import Network.Wreq
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Text.Encoding (encodeUtf8)
import Text.Regex.PCRE
import Data.Array (bounds, (!))

data Cmdline = Cmdline { _cmdlineUser :: Text
                       , _cmdlinePassword :: Text
                       , _cmdlineDirectoryUrl :: Text
                       , _cmdlineDatabaseFile :: FilePath
                       , _cmdlineSeedCode :: Maybe Text
                       , _cmdlineChromeExecutable :: FilePath
                       }
  deriving (Show)

data Config = Config { _configUser :: Text
                     , _configPassword :: Text
                     , _configDirectoryUrl :: Text
                     , _configSeedCode :: Text
                     , _configWebDriver :: WDSession
                     , _configDbh :: SQLite.Connection
                     }



data BackupCode = BackupCode { _backupCodeCode :: Text
                             , _backupCodeUsed :: Bool
                             }
                  deriving (Show)

makeFields ''Config
makeFields ''Cmdline
makeFields ''BackupCode

instance SQLite.FromRow BackupCode where
  fromRow = BackupCode <$> SQLite.field <*> SQLite.field

instance SQLite.ToRow BackupCode where
  toRow (BackupCode code used) = SQLite.toRow (code, bool 0 1 used :: Int)

requiredTextOption name = OptT.textOption (Opt.long name <> Opt.noArgError (Opt.ErrorMsg "required") <> Opt.metavar ("<" <> name <> ">"))

parseArgs :: Opt.Parser Cmdline
parseArgs = Cmdline <$> userOpt <*> passwordOpt <*> urlOpt <*> dbOpt <*> seedCodeOpt <*> chromeBinaryOpt
  where
     userOpt = requiredTextOption "user"
     passwordOpt = requiredTextOption "password"
     urlOpt = requiredTextOption "url"
     dbOpt = Opt.strOption  (Opt.long "db-file" <> Opt.value "./dp.sqlite")
     chromeBinaryOpt = Opt.strOption (Opt.long "chrome" <> Opt.noArgError (Opt.ErrorMsg "required") <> Opt.metavar ("<chrome>"))
     seedCodeOpt = Opt.option (Just <$> OptT.text)
                              (  Opt.long "code"
                              <> Opt.value Nothing )

main = withStderrLogging $ runDaily =<< Opt.execParser (Opt.info parseArgs mempty)

info = Control.Logging.log

runDaily :: Cmdline -> IO ()
runDaily cmdline = do
  withDatabase cmdline $ \db -> do
    withValidSeedCode cmdline db $ \code -> do
      withWebDriver cmdline $ \session -> do
        let config = Config (cmdline^.user) (cmdline^.password) (cmdline^.directoryUrl) code session db
        codesLeft <- countCodesLeft config
        info $ "We have " <> showt codesLeft <> " backup codes left"
        cookie <- runWD session $ acquireSessionId config
        putStrLn $ show cookie
        info $ "Got cookie " <> cookie
        fetchTodaysDirectory config cookie
        bool (return ()) (refreshBackupCodes config) (codesLeft < 5)
        return ()

fetchBackupCodes :: Config -> WD [Text]
fetchBackupCodes config = do
  openPage "https://myaccount.google.com/signinoptions/two-step-verification"
  passwordInput <- findElem $ ByName "password"
  sendKeys (config^.password) passwordInput
  liftIO $ threadDelay 7000000
  nextButton <- findElem $ ById "passwordNext"
  click nextButton
  showCodesButton <- findElem $ ByXPath "//span[text()='Show codes']"
  click showCodesButton
  liftIO $ threadDelay 7000000
  getNewCodesButton <- findElem $ ByXPath "//span[text()='Get new codes']"
  click getNewCodesButton
  liftIO $ threadDelay 7000000
  okButton <- findElem $ ByXPath "(//span[.='OK'])[2]"
  click okButton
  liftIO $ threadDelay 7000000
  codesTable <- findElem $ ByCSS "table"
  raw <- getText codesTable
  return $ filter (T.all isDigit) $ map (T.filter (/= ' ')) $ T.lines raw

refreshBackupCodes :: Config -> IO ()
refreshBackupCodes config = do
  info "Will refresh backup codes"
  codes <- runWD (config^.webDriver) $ fetchBackupCodes config
  SQLite.execute_ (config^.dbh) "delete from backup_codes"
  mapM_ (SQLite.execute (config^.dbh) "replace into backup_codes(code, used) values(?,?)" . (flip BackupCode False)) codes
  info $ "Got new backup codes " <> showt codes
  return ()

fetchTodaysDirectory :: Config -> Text -> IO ()
fetchTodaysDirectory cfg rawCookie = do
  ts <- formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime
  json <- fetchDirectory cfg rawCookie
  BL.writeFile ("directory-" <> ts) json
  return ()

extractToken :: Text -> B.ByteString
extractToken token =
  let matches = (T.unpack token) =~ ("DIRECTORY_TOKEN=([0-9a-f]+)" :: String) :: [MatchText String]
  in case matches of
      [match] -> C8.pack $ fst $ match ! 1
      _ -> error "No directory token in cookie"

fetchDirectory :: Config -> Text -> IO BL.ByteString
fetchDirectory cfg rawCookie = do
  let token = extractToken rawCookie
  let opts = defaults & header "Cookie" .~ [encodeUtf8 rawCookie]
                      & header "X-Directory-Token" .~ [token]
      base = T.unpack $ cfg^.directoryUrl
  r <- getWith opts (base <> "/api/employees")
  let httpCode = r ^. responseStatus . statusCode
  info $ "Fetching directory resulted in http code " <> showt httpCode
  return $ r ^. responseBody


countCodesLeft :: Config -> IO Int
countCodesLeft config = do
  [[cnt]] <- SQLite.query_ (config^.dbh) "select count(*) from backup_codes where used = 0"
  return cnt

withWebDriver :: Cmdline -> (WDSession -> IO a) -> IO a
withWebDriver cmdline ioa = do
  let chromeConfig = useBrowser (chrome {chromeBinary = Just (cmdline^.chromeExecutable)}) defaultConfig
  bracket (runSession chromeConfig getSession) (\sess -> runWD sess closeSession) ioa

withValidSeedCode :: Cmdline -> SQLite.Connection -> (Text -> IO a) -> IO a
withValidSeedCode cmdline dbh ioa = do
  codes :: [BackupCode] <- SQLite.query_ dbh "SELECT * from backup_codes where used = 0 limit 1"
  case codes  of
    [codeObj] -> do
      SQLite.execute dbh "update backup_codes set used = 1 where code = ?" (SQLite.Only $ codeObj^.code)
      info $ "Marked code " <> (codeObj^.code) <> " as used"
      ioa (codeObj^.code)
    _ | cmdline^.seedCode /= Nothing ->
      ioa $ fromJust $ cmdline^.seedCode
    _ ->
      error "No backup codes in the database, and none provided on command-line"

withDatabase :: Cmdline -> (SQLite.Connection -> IO a) -> IO a
withDatabase cmdline ioa = do
  SQLite.withConnection (cmdline^.databaseFile) $ \conn -> do
    ensureTables conn
    ioa conn

ensureTables :: SQLite.Connection -> IO ()
ensureTables conn = do
  SQLite.execute_ conn "CREATE TABLE IF NOT EXISTS backup_codes(code TEXT PRIMARY KEY, used NUMERIC NOT NULL)"

acquireSessionId :: Config -> WD Text
acquireSessionId cfg = do
    setImplicitWait 3000
    openPage $ T.unpack $ cfg^.directoryUrl
    signinButton <- findElem $ ById "signinButton"
    click signinButton
    emailInput <- findElem $ ById "identifierId"
    sendKeys (cfg^.user) emailInput
    submit emailInput
    nextButton <- findElem $ ById "identifierNext"
    click nextButton
    passwordInput <- findElem $ ByName "password"
    sendKeys (cfg^.password) passwordInput
    nextButton <- findElem $ ById "passwordNext"
    liftIO $ threadDelay 1000000
    click nextButton
    skipChallengeLink <- findElem $ ById "skipChallenge"
    click skipChallengeLink
    chooseBackupCodes <- findElem $ ByCSS "form[action=\"/signin/challenge/bc/4\"] button"
    click chooseBackupCodes
    backupCodeInput <- findElem $ ById "backupCodePin"
    sendKeys (cfg^.seedCode) backupCodeInput
    liftIO $ threadDelay 1000000
    submit backupCodeInput
    liftIO $ threadDelay 1000000
    executeJS [] "return window.document.cookie"

-- cont s = withSession s $ do
--      return 1
