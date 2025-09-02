module Wedding.CLI (Options(..), parseOptions) where

import Options.Applicative

-- | Command line options for the wedding server
data Options = Options
  { configFile :: FilePath
  } deriving (Show)

-- | Parser for the --config-file argument
configFileParser :: Parser FilePath
configFileParser = strOption
  ( long "config-file"
  <> metavar "FILE"
  <> help "Path to the YAML configuration file"
  )

-- | Parser for all command line options
optionsParser :: Parser Options
optionsParser = Options <$> configFileParser

-- | Parse command line options with program info
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
      <> progDesc "Wedding website server"
      <> header "wedding - A beautiful wedding website"
      )