module Motorcycle.Storage
  ( StorageCommand(..)
  , StorageSource
  , localStorageSources
  , sessionStorageSources
  , getItem
  , length
  ) where

import DOM.WebStorage.String as WS
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Stream (Stream, constant, startWith, tapEvent)
import Control.Stream.Stream (Subject, combine2, drain, fromSubject, hold, never, skipRepeats, toHoldSubject)
import DOM.WebStorage (ForeignStorage, getLocalStorage, getSessionStorage)
import Data.Function ((#), ($))
import Data.Functor (map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)

data StorageCommand
  = SetItem String String
  | RemoveItem String

type StorageSinks sinks = { storage :: Stream StorageCommand | sinks }

type StorageSources = { storage :: StorageSource }

newtype StorageSource = StorageSource
  { getItem :: String -> Stream (Maybe String)
  , length :: Unit -> Stream Int
  }

localStorage' :: ForeignStorage
localStorage' = unsafePerformEff getLocalStorage

sessionStorage' :: ForeignStorage
sessionStorage' = unsafePerformEff getSessionStorage

localStorage :: Subject ForeignStorage
localStorage = never # constant localStorage' # startWith localStorage' # toHoldSubject 1

sessionStorage :: Subject ForeignStorage
sessionStorage = never # constant sessionStorage' # startWith sessionStorage' # toHoldSubject 1

localStorageSources :: forall sinks. StorageSinks sinks -> StorageSources
localStorageSources = storageSources (fromSubject localStorage)

sessionStorageSources :: forall sinks. StorageSinks sinks -> StorageSources
sessionStorageSources = storageSources (fromSubject sessionStorage)

storageSources :: forall sinks. Stream (ForeignStorage) -> StorageSinks sinks -> StorageSources
storageSources storage sinks = { storage: StorageSource { getItem, length } } where
  x = unsafePerformEff $ combine2 Tuple sinks.storage storage # tapEvent performAction # drain

  getItem key = storage # map (\s -> unsafePerformEff (WS.getItem s key)) # skipRepeats # hold

  length _ = storage # map (\s -> unsafePerformEff (WS.length s)) # skipRepeats # hold

performAction :: Tuple StorageCommand ForeignStorage -> Eff () Unit
performAction (Tuple (SetItem key value) storage) = unsafeCoerceEff $ WS.setItem storage key value
performAction (Tuple (RemoveItem key) storage) =  unsafeCoerceEff $ WS.removeItem storage key

getItem :: String -> StorageSource -> Stream (Maybe String)
getItem key (StorageSource source) = source.getItem key

length :: StorageSource -> Stream Int
length (StorageSource source) = source.length unit

_x :: Unit
_x = unsafePerformEff (drain (fromSubject localStorage))

_y :: Unit
_y = unsafePerformEff (drain (fromSubject sessionStorage))
