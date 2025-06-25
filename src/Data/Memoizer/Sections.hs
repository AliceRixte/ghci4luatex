--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Memoizer.Section
-- Description :  Sections for command memoizers
-- Copyright   :  (c) Alice Rixte 2024
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  portable
--
-- A data structure to memoize the result of the execution of a sequence of
-- commands within some section scope.
--
-- This enables the use of the @\ghcisection{My section}@ command in the @ghci@
-- LuaTex package.
--
-- = Presentation
--
-- When dealing with very long sequences of commands as it may be the case in a
-- long LaTeX document, it can be time consuming to recompute the whole
-- sequence of commands when one modifies one of the early commands.
--
-- In LaTex, we want to be able to say to the server that we only need to
-- recompute the commands within a certain section and to use the memoized
-- commands after.
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

module Data.Memoizer.Sections where

import Data.Memoizer.Commands as Cmd
import qualified Data.Map as Map

data SectionMemoizer k a b = SectionMemoizer
  { sectionMap :: Map.Map k (CmdMemoizer a b)
  , currentSection :: k
  }
  deriving (Show,Eq)

data SectionMemoizerException =
  UndefinedSection

undefinedSection :: String
undefinedSection = "UndefinedSection : The current Section does not exist.\
  \ This should never happen. Please report this as a bug."

lookupCmd :: Ord k => SectionMemoizer k a b -> CmdMemoizer a b
lookupCmd (SectionMemoizer ms k) = case Map.lookup k ms of
  Nothing -> error $ "lookup : " ++ undefinedSection
  Just m -> m

mapCmd :: Ord k =>
  (CmdMemoizer a b -> CmdMemoizer a b)
  -> SectionMemoizer k a b -> SectionMemoizer k a b
mapCmd f sm@(SectionMemoizer ms k) =
  sm {sectionMap = Map.insert k (f (lookupCmd sm)) ms }

-------------------------------------------------------------------------------


initSection :: Ord k => k -> SectionMemoizer k a b
initSection k = SectionMemoizer (Map.insert k Cmd.empty Map.empty) k

newSection :: Ord k => k -> SectionMemoizer k a b -> SectionMemoizer k a b
newSection k (SectionMemoizer ms _) =
  case Map.lookup k ms of
    Nothing -> SectionMemoizer (Map.insert k Cmd.empty ms) k
    Just m -> SectionMemoizer (Map.insert k (Cmd.restart m) ms) k

continueSection :: Ord k => k -> SectionMemoizer k a b -> SectionMemoizer k a b
continueSection k m = m {currentSection = k}

lookup :: (Eq a, Ord k) => a -> SectionMemoizer k a b -> Maybe b
lookup a  = Cmd.lookup a . lookupCmd

nextCmd :: Ord k => SectionMemoizer k a b -> SectionMemoizer k a b
nextCmd = mapCmd Cmd.nextCmd

storeResult :: Ord k => a -> b -> SectionMemoizer k a b -> SectionMemoizer k a b
storeResult a b = mapCmd (Cmd.storeResult a b)

