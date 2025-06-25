--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Memoizer.Session
-- Description :  Sessions for command memoizers
-- Copyright   :  (c) Alice Rixte 2024
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  portable
--
-- Memoize several sessions and switch between them.
--
-- This enables the use of the @\\ghciSession{My Session}@ and
-- @\\ghcicontinue{My Session}@ command in the @ghci@ LuaTex package.
--
-- = Presentation
--
-- When dealing with very long sequences of commands as it may be the case in a
-- long LaTeX document, it can be time consuming to recompute the whole
-- sequence of commands when one modifies one of the early commands.
--
-- In LaTex, we want to be able to say to the server that we only need to
-- recompute the commands within a certain Session and to use the memoized
-- commands after.
--
-- = Usage
--
--
-- Let us store the result of some commands (we alternate between @memo@ and
-- @memo'@ to avoid recursive definitions)
--
-- >>> import Prelude hiding (lookup)
-- >>> memo = initSession "main" :: SessionMemoizer String String String
-- >>> memo' = storeResult "x=1" "" memo
-- >>> memo = storeResult "y=2" "" memo'
-- >>> memo' = storeResult "x+y" "3" memo
--
--
-- >>> memo = newSession "My Session" memo'
-- >>> memo' = storeResult "a=1" "" memo
-- >>> memo = newSession "main" memo'
-- >>> lookup "x=1" memo
-- Just ""
--
-- >>> memo' = newSession "My Session" memo
-- >>> lookup "a=1" memo
-- Just ""
--
--
--------------------------------------------------------------------------------

module Data.Memoizer.Sessions where

import Prelude hiding (lookup)
import qualified Data.Memoizer.Commands as Cmd
import qualified Data.Map as Map

data SessionMemoizer k a b = SessionMemoizer
  { sessionMap :: Map.Map k (Cmd.CmdMemoizer a b)
  , currentSession :: k
  }
  deriving (Show,Eq)

data SessionMemoizerException =
  UndefinedSession

undefinedSession :: String
undefinedSession = "UndefinedSession : The current Session does not exist.\
  \ This should never happen. Please report this as a bug."

lookupCmd :: Ord k => SessionMemoizer k a b -> Cmd.CmdMemoizer a b
lookupCmd (SessionMemoizer ms k) = case Map.lookup k ms of
  Nothing -> error $ "lookup : " ++ undefinedSession
  Just m -> m

mapCmd :: Ord k =>
  (Cmd.CmdMemoizer a b -> Cmd.CmdMemoizer a b)
  -> SessionMemoizer k a b -> SessionMemoizer k a b
mapCmd f sm@(SessionMemoizer ms k) =
  sm {sessionMap = Map.insert k (f (lookupCmd sm)) ms }

-------------------------------------------------------------------------------


initSession :: Ord k => k -> SessionMemoizer k a b
initSession k = SessionMemoizer (Map.insert k Cmd.empty Map.empty) k

newSession :: Ord k => k -> SessionMemoizer k a b -> SessionMemoizer k a b
newSession k (SessionMemoizer ms _) =
  case Map.lookup k ms of
    Nothing -> SessionMemoizer (Map.insert k Cmd.empty ms) k
    Just m -> SessionMemoizer (Map.insert k (Cmd.restart m) ms) k

continueSession :: Ord k => k -> SessionMemoizer k a b -> SessionMemoizer k a b
continueSession k m = m {currentSession = k}

lookup :: (Eq a, Ord k) => a -> SessionMemoizer k a b -> Maybe b
lookup a  = Cmd.lookup a . lookupCmd

nextCmd :: Ord k => SessionMemoizer k a b -> SessionMemoizer k a b
nextCmd = mapCmd Cmd.nextCmd

storeResult :: Ord k => a -> b -> SessionMemoizer k a b -> SessionMemoizer k a b
storeResult a b = mapCmd (Cmd.storeResult a b)

