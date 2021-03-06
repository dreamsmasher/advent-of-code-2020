module API where

import Advent
-- import Data.Function
import Control.Monad.Reader
import Data.Text
import Data.Time.Calendar hiding (Day)
import Data.Time.Clock 
-- import Data.Maybe


type AoCEnv a = ReaderT AoCOpts IO a
type Solution a = (String -> a)

runAdv ::  AoCEnv (Either AoCError String) -> IO ()
runAdv f = do
    key <- readFile ".config/session_cookie.txt"
    res <- runReaderT f (mkOpts key)
    case res of
        Left err -> print err
        Right r -> putStrLn r
-- load key from config file
mkOpts :: String -> AoCOpts
mkOpts key = (defaultAoCOpts 2020 key) {_aCache = Just "./aoc_cache"}

fetchInput :: Day -> AoCEnv (Either AoCError String)
fetchInput d = do
    opts <- ask
    resp <- liftIO (runAoC opts $ AoCInput d)
    pure (unpack <$> resp)

sendToAOC :: (Show a) => Day -> Part -> Solution a -> String -> AoCEnv (Either AoCError String)
sendToAOC d p f t = do
    opts <- ask
    let res = f t
    resp <- liftIO (runAoC opts $ AoCSubmit d p (show res))
    case resp of 
        Left err -> pure (Left err)
        Right res -> pure (Right . showSubmitRes $ snd res) -- drop response code, not needed 
    
getToday :: AoCEnv Day
getToday = do
    date <- (\(_, _, d) -> d) <$> liftIO (pure . toGregorian . utctDay =<< getCurrentTime )
    pure $ mkDay_ (fromIntegral date `mod` 25)

fetchToday :: AoCEnv (Either AoCError String)
fetchToday = do
    today <- getToday
    fetchInput today
    
sendToday :: (Show a) => Part -> Solution a -> String -> AoCEnv (Either AoCError String)
sendToday p f s = do
    today <- getToday
    sendToAOC today p f s
    
-- assuming we've tested everything locally and hope to god that it's right
yolo :: (Show a) => Part -> Solution a -> AoCEnv (Either AoCError String)
yolo p f = do
    input <- fetchToday
    case input of
        Left err -> pure $ Left err
        Right inp -> sendToday p f inp 

    

