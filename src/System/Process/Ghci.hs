{-# LANGUAGE DeriveGeneric #-}

module System.Process.Ghci where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import GHC.Generics

import System.IO
import System.IO.Error
import System.Process


import Data.Aeson

-- | A GHCi process
--
data Ghci = Ghci
  { ghciIn :: Handle
  , ghciErrVar :: TMVar String
  , ghciOutVar :: TMVar String
  , ghciProcess :: ProcessHandle
  , ghciErrId :: ThreadId
  , ghciOutId :: ThreadId
  }


-- | Start a GHCi process. The first argument is the command to run (e.g. "ghci"
-- or "stack") and the second argument is the list of arguments to pass to the
-- command.
--
-- It will return a Ghci process as well as all the warnings, errors and
-- information printed by ghci when it starts.
--
-- NB : If there is an error, GhciResult will still be a GHCi warning. This
-- function does not attempt to parse for errors. The distinction between
-- GhciError and GhciWarning only makes sense when calling @'sendGhciCmd'@
--
-- >>> (ghci, res) = startGhci "ghci" []
--
-- >>> (ghci, res) = startGhci "stack" ["ghci"]
--
startGhci :: String -> [String] -> IO (Ghci, GhciResult)
startGhci cmd args = do
  chans <- createProcess (proc cmd args) { std_in = CreatePipe
                                  , std_err = CreatePipe
                                  , std_out = CreatePipe
                                  }
  case chans of
    (Just hin, Just hout , Just herr , hp) -> do
      verr <- newEmptyTMVarIO
      vout <- newEmptyTMVarIO

      errId  <-forkIO $ listenHandle verr herr
      outId <-forkIO $ listenHandle vout hout

      let g = Ghci hin verr vout hp errId outId

      threadDelay 2000000 -- wait 200 ms to make sure GHCi is ready to listen

      res <- waitGhciResult g

      return (g, res)
    _ -> error "Error : Ghci command failed."

-- | Listen to a handle by putting lines into a TMVar
--
listenHandle :: TMVar String -> Handle -> IO ()
listenHandle v h =
  catch (forever $ do
    s <- hGetLine h
    atomically $ putTMVar v s) handler
  where
    handler :: IOError -> IO ()
    handler err =
      unless (isEOFError err) $ throw err

-- | This is the merged stream of stderr and stdout of ghci.
--
nextErrOrOut :: TMVar String -> TMVar String -> IO (Either String String)
nextErrOrOut verr vout=
  atomically $ (Left <$> takeTMVar verr) `orElse`
      ( Right . cleanResultString <$> takeTMVar vout)



-- | The result
data GhciResult = GhciResult
  { ghciOut :: String
  , ghciErr :: String
  }
  deriving (Show, Eq, Generic)


instance ToJSON GhciResult where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON GhciResult

-- | Sends a command to ghci and wait for its result.
--
sendGhciCmd :: Ghci -> String -> IO GhciResult
sendGhciCmd g cmd = do
  flushGhciCmd (ghciIn g) cmd
  waitGhciResult g

-- | Wait for GHCi to complete its computation
--
waitGhciResult :: Ghci -> IO GhciResult
waitGhciResult g  = do

  -- this is a hack : we send a "putStrLn" command to ghci in order to be able
  -- to wait for the result of the command.
  flushGhciCmd (ghciIn g) $ "putStrLn\"" ++ readyString ++ "\"\n"

  acc <- loop mempty
  return $ toGhciResult acc

  where
    loop acc = do
      s <- nextErrOrOut (ghciErrVar g) (ghciOutVar g)
      if s == Right readyString then
        return acc
      else do
        case s of
          Left s' -> do
            hPutStr stderr s'
            hPutChar stderr '\n'
            hFlush stderr
          Right s' -> do
            putStrLn s'
            hFlush stdout
        loop (appendGhciResult acc s)

-- | Take any string and flushed it in stdin of ghci. Multiple line strings are
-- accepted and surrounded by ":{" and ":}"
--
flushGhciCmd :: Handle -> String -> IO ()
flushGhciCmd hin cmd = do
  hPutStr hin (":{\n" ++ cmd ++ "\n:}\n") -- TODO optimization when no newline ?
  hFlush hin


-- | A very unlikely string that we make ghci print  in order to know when ghci
-- is finished.
--
-- This is a hack, of course, but it works well.
--
readyString :: String
readyString = "`}./*^`a`('))}{h}"


-- Remove the "ghci> " and "ghci| " prefixes from the output stream of ghci.
--
cleanResultString :: String -> String
cleanResultString ('g' : 'h' : 'c' : 'i' : '>': ' ' : s) =
  cleanResultString s
cleanResultString ('g' : 'h' : 'c' : 'i' : '|': ' ' : s) =
  cleanResultString s
cleanResultString s = s

-- | A utility accumulator type that accumulates lines from stdout and stderr.
--
data GhciResultLines = GhciResultLines
  { ghciOutLines :: [String]
  , ghciErrLines :: [String]
  }
  deriving (Show, Eq)

instance Semigroup GhciResultLines where
  GhciResultLines o1 e1 <> GhciResultLines o2 e2=
    GhciResultLines (o1 ++ o2) (e1 ++ e2)

instance Monoid GhciResultLines where
  mempty = GhciResultLines [] []




-- | Utility function that allows to merge the stderr and the stdout streams of
-- ghci. When both streams are active after sending a command, this is because
-- ghci is issuing a warning.
--
-- Beware : for performance reasons, the lists are reversed.
--
appendGhciResult :: GhciResultLines -> Either String String -> GhciResultLines
appendGhciResult acc (Left s) =
  acc { ghciErrLines = s : ghciErrLines acc }
appendGhciResult acc (Right s) =
  acc { ghciOutLines = s : ghciOutLines acc }

-- | Concatenates the string by adding a newline '\n' between them.
--
-- For some reason Prelude's unlines adds an extra newline character at the
-- end. This version of unlines doesn't.
-- unlines' :: [String] -> String
-- unlines' [] = ""
-- unlines' [s] = s
-- unlines' (l:ls) = l ++ '\n' : unlines' ls

-- | Converts Ghci accumulated lines to a proper result.
--
toGhciResult :: GhciResultLines -> GhciResult
toGhciResult (GhciResultLines o e) =
  GhciResult (unlines o) (unlines e)
