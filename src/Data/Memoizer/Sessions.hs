module Data.Memoizer.Sessions where



import Data.Memoizer.Commands as Cmd
import qualified Data.Map as Map

data SessionMemoizer k a b = SessionMemoizer
  { sessionMap :: Map.Map k (CmdMemoizer a b)
  , currentSession :: k
  }
  deriving (Show,Eq)

data SessionMemoizerException =
  UndefinedSession

undefinedSession :: String
undefinedSession = "UndefinedSession : The current session does not exist.\
  \ This should never happen. Please report this as a bug."

lookupCmd :: Ord k => SessionMemoizer k a b -> CmdMemoizer a b
lookupCmd (SessionMemoizer ms k) = case Map.lookup k ms of
  Nothing -> error $ "lookup : " ++ undefinedSession
  Just m -> m

mapCmd :: Ord k =>
  (CmdMemoizer a b -> CmdMemoizer a b)
  -> SessionMemoizer k a b -> SessionMemoizer k a b
mapCmd f sm@(SessionMemoizer ms k) =
  sm {sessionMap = Map.insert k (f (lookupCmd sm)) ms }

--------------------------------------------------------------------------------

initSession :: Ord k => k -> SessionMemoizer k a b
initSession k = SessionMemoizer (Map.insert k Cmd.empty Map.empty) k

newSession :: Ord k => k -> SessionMemoizer k a b -> SessionMemoizer k a b
newSession k (SessionMemoizer ms _) =
  case Map.lookup k ms of
    Nothing -> SessionMemoizer (Map.insert k Cmd.empty ms) k
    Just m -> SessionMemoizer (Map.insert k (Cmd.restartSession m) ms) k

lookup :: (Eq a, Ord k) => a -> SessionMemoizer k a b -> Maybe b
lookup a  = Cmd.lookup a . lookupCmd

nextCmd :: Ord k => SessionMemoizer k a b -> SessionMemoizer k a b
nextCmd = mapCmd Cmd.nextCmd

storeResult :: Ord k => a -> b -> SessionMemoizer k a b -> SessionMemoizer k a b
storeResult a b = mapCmd (Cmd.storeResult a b)

