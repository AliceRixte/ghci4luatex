--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Memoizer.Commands
-- Description :  A memoizer for sequences of commands
-- Copyright   :  (c) Alice Rixte 2024
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  portable
--
-- A data structure to memoize the result of the execution of a sequence of
-- commands.
--
-- As long as the sequence of commands is identical to the one memoized, we can
-- avoid executing them. As soon as there is one command that defers from the
-- memoized sequence, then we should discard all remaining memoized results and
-- execute all the subsequent commands.
--
-- = Usage
--
--
-- Let us store the result of some commands (we alternate between @memo@ and
-- @memo'@ to avoid recursive definitions)
--
-- >>> import Prelude hiding (lookup)
-- >>> memo' = storeResult "x=1" "" empty :: CmdMemoizer String String
-- >>> memo = storeResult "y=2" "" memo'
-- >>> memo' = storeResult "x+y" "3" memo
--
--
--
-- Suppose there are no more commands, and we want to rerun a sequence of
-- commands, and use the memoized value as long as the commands are identical :
--
-- >>> memo = restart memo'
-- >>> lookup "x=1" memo
-- Just ""
--
-- Since the command was memoized, we avoid executing is again. Now suppose the command @"y=2"@ by @"y=3"@
--
-- >>> memo' = nextCmd memo
-- >>> lookup "y=3" memo'
-- Nothing
--
-- Since the command was not memoized, we have to execute it :
--
-- >>> memo = storeResult "y=3" "" memo'
--
-- Now none the subsequent commands will not use the memoized version :
--
-- >>> memo' = nextCmd memo
-- >>> lookup "x+y" memo'
-- Nothing
--
--------------------------------------------------------------------------------

module Data.Memoizer.Commands
  ( CmdMemoizer
  , empty
  , storeResult
  , lookup
  , restart
  , deleteResult
  , nextCmd
  )
where


import Prelude hiding (lookup)

import qualified Data.IntMap as Map

-- | A container for the memoized result of the execution of a sequence of
-- commands
--
-- * @a@ is the type of commands
-- * @b@ is the type of results
--
data CmdMemoizer a b = CmdMemoizer
  { memoizerMap :: Map.IntMap (a,b)
  , currentIndex :: Int
  , foundModif :: Bool
  }
  deriving (Show, Eq)

-- | The memoized results for an empty sequence of commands
--
empty :: CmdMemoizer a b
empty = CmdMemoizer Map.empty 0 False

-- | Restart the sequence of commands
--
-- Memoized results will now be accessible until @'storeResult'@ or
-- @'deleteResult'@ are used.
--
restart :: CmdMemoizer a b -> CmdMemoizer a b
restart m = m {currentIndex = 0, foundModif = False}


-- | Store the result of the execution of a command.
--
-- This will override the current memoized command, and prevent to access any
-- memoized command until @'restart'@ is used.
--
storeResult :: a -> b -> CmdMemoizer a b -> CmdMemoizer a b
storeResult a b (CmdMemoizer m i _) =
  CmdMemoizer (Map.insert i (a,b) m) (i+1) True

-- | Deletes the result of the current memoized command.
--
-- This will override the current memoized command, and prevent to access any
-- memoized command until @'restart'@ is used.
--
deleteResult :: CmdMemoizer a b -> CmdMemoizer a b
deleteResult (CmdMemoizer m i _) = CmdMemoizer (Map.delete i m) (i+1) True


-- | Access the memoized result of a command if it is equal to the current
-- memoized command
--
lookup :: Eq a => a -> CmdMemoizer a b -> Maybe b
lookup a (CmdMemoizer m i modif) =
  if modif then
    Nothing
  else
    case Map.lookup i m of
      Nothing -> Nothing
      Just (a', b) ->
        if a == a' then
          Just b
        else
          Nothing

-- | Move to the next memoized command
--
nextCmd :: CmdMemoizer a b -> CmdMemoizer a b
nextCmd m = m {currentIndex = currentIndex m + 1 }


