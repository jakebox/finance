import Finance.Commands
import Options.Applicative

mainParser :: Parser Command
mainParser =
  subparser
    ( command
        "report"
        ( info
            (helper <*> (ReportCommand <$> reportCommandParser))
            (progDesc "Generate a report")
        )
    )

main :: IO ()
main = do
    cmd <- execParser (info mainParser (fullDesc <> progDesc "Financial Tracker CLI"))
    case cmd of
        ReportCommand opts -> runReport opts