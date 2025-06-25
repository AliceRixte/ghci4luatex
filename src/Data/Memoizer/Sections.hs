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

lookup :: (Eq a, Ord k) => a -> SectionMemoizer k a b -> Maybe b
lookup a  = Cmd.lookup a . lookupCmd

nextCmd :: Ord k => SectionMemoizer k a b -> SectionMemoizer k a b
nextCmd = mapCmd Cmd.nextCmd

storeResult :: Ord k => a -> b -> SectionMemoizer k a b -> SectionMemoizer k a b
storeResult a b = mapCmd (Cmd.storeResult a b)

