{-# LANGUAGE LambdaCase #-}
module CLI where

import System.Console.Haskeline
import API
import Control.Monad.IO.Class
import Prelude hiding (interact)
import Data.Maybe
import Solutions.Exports

greeting :: String
greeting = "    _          ____\n   / \\   ___  / ___|\n  / _ \\ / _ \\| |\n / ___ \\ (_) | |___\n/_/   \\_\\___/ \\____|\n\n"

-- TODO make this work
-- interact :: (MonadIO m) => InputT m ()
-- interact = do
--     ln <- getInputLine "Enter cmd."
--     case ln of 
--        Nothing -> interact
--        Just l -> parseLine l >> interact

parseLine :: String -> InputT m ()
parseLine s = let w = words s
                  cmd = fromMaybe "" $ listToMaybe w 
              in case cmd of
                  "today" -> undefined

runAdvent :: String -> IO ()
runAdvent apiKey = do
    let opts = mkOpts apiKey
    -- runInputT defaultBehavior $ do   
    return ()
    