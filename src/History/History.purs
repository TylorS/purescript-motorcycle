module Motorcycle.History
  ( Path(..)
  , HistoryInput(..)
  , Location
  , historySources
  )
  where

import Control.Monad.Eff (Pure)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Stream (Stream, drain, tapEvent)
import Data.Function ((#))
import Data.Unit (Unit, unit)

newtype Path = Path String

type Location =
  { pathname :: String
  , search :: String
  , hash :: String
  , key :: String
  }

type History =
  { history :: Stream Location
  , push :: String -> Pure Unit
  , replace :: String -> Pure Unit
  , go :: Int -> Pure Unit
  }

data HistoryInput
  = Push Path
  | Replace Path
  | Go Int

historySources :: forall sinks. { history :: Stream HistoryInput | sinks } -> { history :: Stream Location }
historySources sinks = { history: h.history } where
  h :: History
  h = createHistory unit

  input :: HistoryInput -> Pure Unit
  input x = case x of
    Push (Path path) -> h.push path
    Replace (Path path) -> h.replace path
    Go amount -> h.go amount

  x' :: Unit
  x' = unsafePerformEff (sinks.history # tapEvent input # drain)

foreign import createHistory :: Unit -> History
