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
        <> command
          "r"
          ( info
              (helper <*> (ReportCommand <$> reportCommandParser))
              (progDesc "Alias for report")
          )
        <> command
          "budget"
          ( info
              (helper <*> (BudgetCommand <$> budgetCommandParser))
              (progDesc "Budgets")
          )
        <> command
          "tx"
          ( info
              (helper <*> (TransactionCommand <$> transactionCommandParser))
              (progDesc "Transactions")
          )
        <> command
          "tui"
          ( info
              (helper <*> (TUICommand <$> tuiCommandParser))
              (progDesc "TUI")
          )
    )

main :: IO ()
main = do
  cmd <-
    execParser (info (mainParser <**> helper) (fullDesc <> progDesc "Financial Tracker CLI"))
  case cmd of
    ReportCommand opts -> runReport opts
    BudgetCommand opts -> runBudget opts
    TransactionCommand opts -> runTransaction opts
    TUICommand opts -> runTUI opts
