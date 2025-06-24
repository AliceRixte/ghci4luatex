{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import GHC.Generics

import System.Process.Ghci

import Network.Simple.TCP

import qualified Data.ByteString.Char8 as B

import System.Console.CmdArgs

import Data.Aeson

data Ghci4luatexMsg = GhciMsg String | LuatexMsg String
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
      serve (Host addr) prt $ \(sock, remoteAddr) -> do
        putStrLn $ "New connection of " ++ show remoteAddr
        handleClient ghci sock

handleClient :: Ghci -> Socket -> IO ()
handleClient ghci sock =
    loop
    where
      loop = do
        msg <- recv sock 1024
        case msg of
            Just bs -> do
                putChar '\n'
                if B.null bs then
                  return ()
                else do
                  case decodeStrict bs :: Maybe Ghci4luatexMsg of
                    Nothing ->
                      let json = encode (GhciResult "" "ghci4luatex :: \
                              \ Error :: Could not parse JSON message.")
                      in
                        sendLazy sock json
                    Just (GhciMsg s) -> do
                      case lines s of
                          [] -> return ()
                          (x:q) -> do
                            putStrLn $ "ghci> " ++ x
                            mapM_ (putStrLn . ("ghci| " ++)) q
                      res <- sendGhciCmd ghci s
                      print res
                      let json = encode res <> "\n"
                      sendLazy sock json -- $ BL.pack(show res ++ "\n")
                    _ -> error "LuatexMsg"
                  loop
            Nothing -> putStrLn "Connexion fermée"
