module Data.Memoizer.Commands where

import qualified Data.IntMap as Map


data CmdMemoizer a b = CmdMemoizer
  { memoizerMap :: Map.IntMap (a,b)
  , currentIndex :: Int
  , foundModif :: Bool
  }
  deriving (Show, Eq)


empty :: CmdMemoizer a b
empty = CmdMemoizer Map.empty 0 False


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

restart :: CmdMemoizer a b -> CmdMemoizer a b
restart m = m {currentIndex = 0, foundModif = False}

storeResult :: a -> b -> CmdMemoizer a b -> CmdMemoizer a b
storeResult a b (CmdMemoizer m i _) =
  CmdMemoizer (Map.insert i (a,b) m) (i+1) True

deleteResult :: CmdMemoizer a b -> CmdMemoizer a b
deleteResult (CmdMemoizer m i _) = CmdMemoizer (Map.delete i m) (i+1) True

nextCmd :: CmdMemoizer a b -> CmdMemoizer a b
nextCmd m = m {currentIndex = currentIndex m + 1 }


