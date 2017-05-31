{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data Cmdline = Cmdline { user :: Text
                       , password :: Text
                       , directoryUrl :: Text
                       , seedCode :: Maybe Text
                       }
  deriving (Show)

requiredTextOption name = OptT.textOption (Opt.long name <> Opt.noArgError (Opt.ErrorMsg "required") <> Opt.metavar ("<" <> name <> ">"))

parseArgs :: Opt.Parser Cmdline
parseArgs = Cmdline <$> userOpt <*> passwordOpt <*> urlOpt <*> seedCodeOpt
  where
     userOpt = requiredTextOption "user"
     passwordOpt = requiredTextOption "password"
     urlOpt = requiredTextOption "url"
     seedCodeOpt = Opt.option (Just <$> OptT.text)
                              (  Opt.long "code"
                              <> Opt.value Nothing )

main = runDaily =<< Opt.execParser (Opt.info parseArgs mempty)

     -- need at least one valid backup code (either from cmdline, or stored in database)
     -- fetch cookie, logging to google by the way
     -- cache cookie in database
     -- if there is not enough codes, refresh them (we are already logged to google to) and save to database

runDaily :: Cmdline -> IO ()
runDaily cfg = do
  putStrLn $ show cfg
  return ()


data BackupCode = BackupCode { code :: Text
                             , used :: Bool
                             }


data Config = Config
              { login :: Text
              , password :: Text
              , backupCode :: Text
              }

firefoxConfig :: WDConfig
firefoxConfig = defaultConfig

chromeConfig = useBrowser (chrome {chromeBinary = Just "/nix/store/aa7q0mgccyawh05qsqcd85mn65gxyzgn-google-chrome-beta-58.0.3029.68/bin/google-chrome-beta"}) defaultConfig



fetchDirectory (Config{..}) =
  runSession chromeConfig $ do
    setImplicitWait 3000
    openPage "YOU_SHOULD_KNOW"
    signinButton <- findElem $ ById "signinButton"
    click signinButton
    emailInput <- findElem $ ById "identifierId"
    sendKeys login emailInput
    submit emailInput
    nextButton <- findElem $ ById "identifierNext"
    click nextButton
    passwordInput <- findElem $ ByName "password"
    sendKeys password passwordInput
    nextButton <- findElem $ ById "passwordNext"
    liftIO $ threadDelay 1000000
    click nextButton
    skipChallengeLink <- findElem $ ById "skipChallenge"
    click skipChallengeLink
    chooseBackupCodes <- findElem $ ByCSS "form[action=\"/signin/challenge/bc/4\"] button"
    click chooseBackupCodes
    backupCodeInput <- findElem $ ById "backupCodePin"
    sendKeys backupCode backupCodeInput
    liftIO $ threadDelay 1000000
    submit backupCodeInput
    liftIO $ threadDelay 1000000
    getSession

cont s = withSession s $ do
     return 1
