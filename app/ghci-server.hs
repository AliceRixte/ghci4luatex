{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Process.Ghci


import Network.Simple.TCP

import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Aeson

main :: IO ()
main = do
  putChar '\n'
  putStrLn "(-: Starting GHCi Server :-)"
  putChar '\n'
  (ghci, _) <- startGhci "stack" ["ghci"]
  putChar '\n'
  putStrLn "(-: GHCi server is ready :-)"
  putChar '\n'
  serve (Host "127.0.0.1") "54321" $ \(sock, remoteAddr) -> do
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
                  let s = B.unpack bs
                  case lines s of
                      [] -> return ()
                      (x:q) -> do
                        putStrLn $ "ghci> " ++ x
                        mapM_ (putStrLn . ("ghci| " ++)) q
                  res <- sendGhciCmd ghci s
                  print res
                  let json = encode res <> "\n"
                  sendLazy sock json -- $ BL.pack(show res ++ "\n")
                  loop
            Nothing -> putStrLn "Connexion fermée"
