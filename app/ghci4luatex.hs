{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import GHC.Generics
import System.IO
import Data.IORef



import Network.Simple.TCP

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import System.Console.CmdArgs

import Data.Aeson

import System.Process.Ghci
import qualified Data.Memoizer.Sessions as Memo

newtype ServerMsg = NewSession String
  deriving (Show, Eq, Generic)

instance ToJSON ServerMsg where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerMsg

data Ghci4luatexMsg = GhciMsg String | ServerMsg ServerMsg
  deriving (Show, Eq, Generic)


instance ToJSON Ghci4luatexMsg where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Ghci4luatexMsg

data Ghci4luatex = Ghci4luatex
  { command  :: String
  , host :: String
  , port :: String
  }
  deriving (Data,Typeable,Show,Eq)

type GhciMemo =  Memo.SessionMemoizer String String BL.ByteString

cmdArg :: Ghci4luatex
cmdArg =  Ghci4luatex
  { command = "ghci" &= help "Command to run (defaults to ghci)"
  , host = "127.0.0.1" &= help "Host address (defaults to localhost)"
  , port = "54123" &= help "Port (defaults to 54123)"
  }
  &= summary "ghci4luatex v0.1, (C) Alice Rixte"


main :: IO ()
main = do
  Ghci4luatex str addr prt <- cmdArgs cmdArg
  case words str of
    [] -> putStrLn "Invalid ghci command."
    cmd : ghciArgs -> do
      putChar '\n'
      putStrLn "(-: Starting GHCi Server :-)"
      putChar '\n'
      (ghci, _) <- startGhci cmd ghciArgs
      putChar '\n'
      putStrLn "(-: GHCi server is ready :-)"
      putChar '\n'

      memo <- newIORef (Memo.initSession "main" :: GhciMemo)
      serve (Host addr) prt $ \(sock, remoteAddr) -> do
        putStrLn $ "New connection of " ++ show remoteAddr
        handleClient sock ghci memo

printGhciMsg :: String -> IO ()
printGhciMsg s =
  case lines s of
    [] -> return ()
    (x:q) -> do
      putStrLn $ "ghci> " ++ x
      mapM_ (putStrLn . ("ghci| " ++)) q

-- execGhciMsg :: Ghci -> IORef GhciMemo -> String -> IO BL.ByteString
-- execGhciMsg ghci memo s = do
--   m <-readIORef memo


--   res <- sendGhciCmd ghci s
--   print res
--   return  (encode res <> "\n")

handleClient :: Socket ->  Ghci -> IORef GhciMemo ->  IO ()
handleClient sock ghci memo =
    loop
    where
      loop = do
        msg <- recv sock 1024
        case msg of
            Just bs -> do
              putStrLn $ show msg
              putChar '\n'
              if B.null bs then
                return ()
              else do
                case decodeStrict bs :: Maybe Ghci4luatexMsg of
                  Nothing ->
                    let json = encode (GhciResult "" "ghci4luatex :: Error : Could not parse JSON message.")
                    in do
                      hPutStr stderr "Error : Could not parse JSON message."
                      hPutStr stderr "\n"
                      hFlush stderr
                      sendLazy sock json
                  Just (GhciMsg s) -> do
                    printGhciMsg s
                    m <-readIORef memo
                    json <- case Memo.lookup s m of
                      Nothing -> do
                        res <- sendGhciCmd ghci s
                        let json = encode res <> "\n"
                        modifyIORef memo (Memo.storeResult s json)
                        return json
                      Just json -> do
                        putStrLn "Memoized !"
                        modifyIORef memo Memo.nextCmd
                        return json


                    print json
                    sendLazy sock json -- $ BL.pack(show res ++ "\n")
                  Just (ServerMsg (NewSession s)) -> do
                    modifyIORef memo (Memo.newSession s)
                    putStrLn $ "NewSession : " ++ show s

                loop
            Nothing -> putStrLn "Connexion was closed"
