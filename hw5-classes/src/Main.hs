import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano
import           Language.Nano.Repl
import           Text.Printf
import           GHC.IO.Encoding


-- https://util.unicode.org/UnicodeJsps/character.jsp?a=03BB
lambda :: String
lambda = "\x03BB"

main :: IO ()                             
main = do
  setLocaleEncoding utf8
  replLoop 0 Nano.prelude

replLoop :: Int -> Nano.Env -> IO ()
replLoop count env = do
  putStrFlush $ welcome ++ printf "\n%s [%d] " lambda count
  line <- getLine
  case strCmd line of
    CEval expr -> doEval env expr >> replLoop (count + 1) env
    CQuit -> doQuit
--replLoop :: Int -> Nano.Env -> IO ()
--replLoop count env = do
--  putStrFlush $ welcome ++ printf "\n%s [%d] " lambda count
--  line <- getLine
--  case strCmd line of
--    CQuit     -> doQuit

--------------------------------------------------------------------------------
-- | Some useful functions 
--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   :: 
-- putStrLn :: String -> IO ()
-- getLine  :: IO String 
