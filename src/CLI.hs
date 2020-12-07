module CLI where

import System.Console.Haskeline
import API
import Prelude hiding (interact)


greeting :: String
greeting = "    _          ____\n   / \\   ___  / ___|\n  / _ \\ / _ \\| |\n / ___ \\ (_) | |___\n/_/   \\_\\___/ \\____|\n\n"

interact :: InputT m a
interact = do
    ln <- getInputLine "Enter cmd."
    case ln of 
       Nothing -> interact
       Just l -> parseLine l >>= interact

runAdvent :: String -> IO ()
runAdvent apiKey = do
    let opts = mkOpts apiKey
    -- runInputT defaultBehavior $ do   
    return ()