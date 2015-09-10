import Scheme
import System.Console.Readline

repl :: IO ()
repl = do
  maybeLine <- readline "> "
  case maybeLine of
    Nothing -> putStrLn "" -- so the user gets a clean prompt after exit
    Just line -> do
      addHistory line
      let result = evaluate <$> parse line
      case result of
        Left err -> putStrLn err
        Right ast -> putStrLn $ unparse ast
      repl

main :: IO ()
main = repl
