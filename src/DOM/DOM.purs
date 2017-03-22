module Motorcycle.DOM
  ( domSources
  , select
  , elements
  , events
  , CssSelector(..)
  , DomSource
  ) where

import Control.Stream.Stream as S
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Stream (Stream, drain, multicast, scan, skipRepeatsWith, switch, take)
import Control.Stream.Stream (hold)
import DOM.Event.Event (Event)
import DOM.Event.Types (EventTarget, EventType(..))
import DOM.Node.ParentNode (querySelector, querySelectorAll)
import DOM.Node.Types (Element, NodeList, elementToParentNode)
import Data.Array (filter, last, length, snoc)
import Data.Eq ((==))
import Data.Function (($), (#))
import Data.Functor (map)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Nullable (toMaybe)
import Data.StrMap.ST (STStrMap, new, poke, peek)
import Data.String (Pattern(..), indexOf, joinWith)
import Data.Unit (Unit)
import MostlyDom (VNode(..), elementToVNode, patch)
import Partial.Unsafe (unsafePartial)
import Prelude (unit)
import Unsafe.Coerce (unsafeCoerce)

scope_prefix :: String
scope_prefix = "$$MOTORCYCLEDOM$$-"

newtype CssSelector = CssSelector String
newtype Namespace = Namespace (Array String)

newtype Scope = Scope String
type ScopeMap = STStrMap Scope (Stream Event)

newtype EventDelegator = EventDelegator
  { addEventListener :: (Element -> Stream Event) -> EventType -> Scope -> Element -> Stream Event
  }

domSources :: forall sinks. Element -> { view :: Stream VNode | sinks } -> { dom :: DomSource }
domSources root sinks = do
  let rootVNode = elementToVNode root
  let rootVNodeStream = scan patch rootVNode sinks.view
  let rootElementStream = hold $ map (\vNode -> unsafePartial (getElementFromVNode vNode)) rootVNodeStream
  let x = rootElementStream # drain
  let eventDelegator = createEventDelegator unit
  { dom: (createDomSource rootElementStream (Namespace []) eventDelegator) }

getElementFromVNode :: Partial => VNode -> Element
getElementFromVNode (ElementVNode { element }) = case element of Just element' -> element'

createEventDelegator :: Unit -> EventDelegator
createEventDelegator _ = EventDelegator { addEventListener } where
  eventMap :: STStrMap EventType ScopeMap
  eventMap = performEff new

  findScopeMap :: EventType -> ScopeMap
  findScopeMap (EventType eventType) = do
    let maybeScopeMap = performEff (peek eventMap eventType)
    case maybeScopeMap of
      Just scopeMap -> scopeMap
      Nothing -> do
        let map = performEff new
        let x = addScopeMap (EventType eventType) map
        map

  addScopeMap :: EventType -> ScopeMap -> STStrMap EventType ScopeMap
  addScopeMap (EventType eventType) scopeMap = performEff (poke eventMap eventType scopeMap)

  addScope :: Scope -> ScopeMap -> Stream Event -> ScopeMap
  addScope (Scope scope) map event = performEff (poke map scope event)

  addEventListener :: (Element -> Stream Event) -> EventType -> Scope -> Element -> Stream Event
  addEventListener createEventFromElement eventType (Scope scope) element = do
    let scopeMap = findScopeMap eventType
    let maybeEventStream = performEff (peek scopeMap scope)
    case maybeEventStream of
      Just stream -> stream
      Nothing -> do
        let eventStream = createEventFromElement element
        let x = addScope (Scope scope) scopeMap eventStream
        eventStream

newtype DomSource = DomSource
  { select :: CssSelector -> DomSource
  , elements :: Unit -> (Stream (Array Element))
  , events :: EventType -> Boolean -> Stream Event
  }

select :: CssSelector -> DomSource -> DomSource
select selector (DomSource source) = source.select selector

elements :: DomSource -> Stream (Array Element)
elements (DomSource source) = source.elements unit

events :: EventType -> Boolean -> DomSource -> Stream Event
events eventType useCapture (DomSource source) = source.events eventType useCapture

createDomSource :: Stream Element -> Namespace -> EventDelegator -> DomSource
createDomSource rootElement (Namespace namespace) eventDelegator =
  DomSource { elements: elements', events: events', select: select' } where
    elements' :: Unit -> (Stream (Array Element))
    elements' _ = domSourceElements (Namespace namespace) rootElement

    events' :: EventType -> Boolean -> Stream Event
    events' = domSourceEvents (Namespace namespace) rootElement eventDelegator

    select' :: CssSelector -> DomSource
    select' (CssSelector selector) = createDomSource rootElement (Namespace (snoc namespace selector)) eventDelegator

domSourceElements :: Namespace -> Stream Element -> Stream (Array Element)
domSourceElements (Namespace namespace) rootElement = do
  let scope = generateScope (Namespace namespace)
  let selector = generateSelector (Namespace namespace)
  if (length namespace == 0)
    then map (\x -> [x]) rootElement
    else if (emptySelector selector)
      then rootElement # map (findMostSpecificElement scope) # map (\x -> [x])
      else rootElement # map (findMatchingElements selector (isInScope scope))

domSourceEvents :: Namespace -> Stream Element -> EventDelegator -> EventType -> Boolean -> Stream Event
domSourceEvents (Namespace namespace) rootElement delegator eventType useCapture = do
  let scope = generateScope (Namespace namespace)
  let selector = generateSelector (Namespace namespace)
  if (length namespace == 0)
    then rootElement # take 1 # map (domEvent eventType useCapture) # switch # multicast
    else rootElement
      # map (findMostSpecificElement scope)
      # skipRepeatsWith sameElement
      # map (createEventStream delegator eventType useCapture scope selector)
      # switch
      # multicast

createEventStream :: EventDelegator -> EventType -> Boolean -> Scope -> CssSelector -> Element -> Stream Event
createEventStream (EventDelegator delegator) eventType useCapture scope selector element =
  scopeEventStream (delegator.addEventListener (domEvent eventType useCapture) eventType scope element) (isInScope scope) selector element

emptySelector :: CssSelector -> Boolean
emptySelector (CssSelector selector) = selector == ""

findMostSpecificElement :: Scope -> Element -> Element
findMostSpecificElement (Scope scope) rootElement = do
  let elem = querySelector' (CssSelector scope) rootElement
  case elem of
    Just e -> e
    Nothing -> rootElement

findMatchingElements :: CssSelector -> (Element -> Boolean) -> Element -> Array Element
findMatchingElements selector checkIsInScope element = do
  let matchedNodes = filter checkIsInScope (querySelectorAll' selector element)
  if (matches selector element)
    then snoc matchedNodes element
    else matchedNodes

scopeEventStream :: Stream Event -> (Element -> Boolean) -> CssSelector -> Element -> Stream Event
scopeEventStream event predicate selector rootElement = event
  # S.filter (\x -> predicate $ eventToElement x)
  # S.filter (\x -> ensureMatches selector rootElement x)
  # S.multicast

generateSelector :: Namespace -> CssSelector
generateSelector (Namespace ns) = CssSelector (joinWith " " (filter findSelector ns))

findSelector :: String -> Boolean
findSelector selector = if (findScope selector) then false else true

generateScope :: Namespace -> Scope
generateScope (Namespace ns) = do
  let maybeScope = map (\x -> Scope x) $ last (filter findScope ns)
  case maybeScope of
    Just scope -> scope
    Nothing -> Scope ""

findScope :: String -> Boolean
findScope selector = do
  let index = indexOf (Pattern scope_prefix) selector
  case index of
    Just i -> i == 0
    Nothing -> false

querySelector' :: CssSelector -> Element -> Maybe Element
querySelector' (CssSelector selector) element = toMaybe $ unsafePerformEff $ unsafeCoerceEff $
  querySelector selector (elementToParentNode element)

querySelectorAll' :: CssSelector -> Element -> Array Element
querySelectorAll' (CssSelector selector) element = nodeListToArray $ unsafePerformEff $ unsafeCoerceEff $
  querySelectorAll selector (elementToParentNode element)

eventTargetToElement :: EventTarget -> Element
eventTargetToElement = unsafeCoerce

performEff :: forall eff a. Eff (| eff) a -> a
performEff f = unsafePerformEff (unsafeCoerceEff f)

foreign import nodeListToArray :: NodeList -> Array Element
foreign import matches :: CssSelector -> Element -> Boolean
foreign import ensureMatches :: CssSelector -> Element -> Event -> Boolean
foreign import eventToElement :: Event -> Element
foreign import isInScope :: Scope -> Element -> Boolean
foreign import domEvent :: EventType -> Boolean -> Element -> Stream Event
foreign import sameElement :: Element -> Element -> Boolean
