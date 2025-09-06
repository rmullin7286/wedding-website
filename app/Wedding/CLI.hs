module Wedding.CLI (Options (..), parseOptions) where

import Options.Applicative

-- | Command line options for the server
data Options = Options
  deriving (Show)

-- | Parser for all command line options
optionsParser :: Parser Options
optionsParser = pure Options

-- | Parse command line options with program info
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Wedding website server"
            <> header "Wedding website - for Ryan and Shae"
        )
