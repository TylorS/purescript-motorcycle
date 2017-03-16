module Motorcycle.Run (run) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Stream (Disposable, EffStream)

run :: forall sources sinks e. ({ | sources } -> { | sinks }) -> ({ | sinks } -> { | sources }) -> EffStream e Disposable
run main effects = createSubscriptions sinks proxySinks where
  proxySinks :: { | sinks }
  proxySinks = unsafePerformEff createSinks

  sinks :: { | sinks}
  sinks = main (effects proxySinks)

foreign import createSinks :: forall sinks e. Eff e { | sinks }
foreign import createSubscriptions :: forall sinks e. { | sinks} -> { | sinks} -> EffStream e Disposable
