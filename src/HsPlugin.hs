{-# LANGUAGE TemplateHaskell, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module HsPlugin (HsPluginState,
                 HsPluginE,
                 PluginState,
                 dispatchEvent,
                 subscribeEvent,
                 getState,
                 getState',
                 putState,
                 interpretHsPlugin,
                 initialize,
                 initializeWithListeners,
                 initializeWithPlugins,
                 ListenerMap,
                 makeListenerMap,
                 initializeState,
                 Listener(..)) where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Typeable
import Data.Maybe (fromJust)

import Effectful.State.Static.Shared
import Effectful
import Effectful.TH
import Effectful.Dispatch.Dynamic (reinterpret)

import Control.Monad

data AnyTypeable = forall a. Typeable a => AnyTypeable a

newtype TypeMap = TypeMap (Map TypeRep AnyTypeable)

data Listener a = forall b. Typeable b => Listener {handler :: b -> a,proxy :: Proxy b}

newtype ListenerMap a = ListenerMap (Map TypeRep [Listener a])

makeListenerMap :: [Listener a] -> ListenerMap a
makeListenerMap ls = ListenerMap $ M.fromListWith (++) $ map (\case
   l@(Listener _ p) -> (typeRep  p,[l])
  ) ls

getValueOfType :: Typeable a => Proxy a -> TypeMap -> Maybe a
getValueOfType proxy' (TypeMap typeMap) =
  (\case AnyTypeable a -> fromJust $ cast a) <$> M.lookup (typeRep proxy') typeMap

setValueInMap :: Typeable a => TypeMap -> a -> TypeMap
setValueInMap (TypeMap typeMap) value =
  TypeMap $ M.insert (typeOf value) (AnyTypeable value) typeMap

getListenersValues :: Typeable b => ListenerMap a -> b -> [a]
getListenersValues (ListenerMap listenerMap) event =
  maybe [] (map (\case
         Listener run _ -> run $ fromJust $ cast event))
     (M.lookup (typeOf event) listenerMap)

addListener :: Typeable a => Proxy a -> (a -> b) -> ListenerMap b -> ListenerMap b
addListener proxy' f (ListenerMap m) =
  ListenerMap $ M.insertWith (++) (typeRep proxy') [Listener f proxy'] m

data HsPluginState m = HsPluginState {pluginStates :: TypeMap, listeners :: ListenerMap (m ())}

class PluginState a where
    initializeState :: Eff (HsPluginE es : es) a

data HsPluginE es :: Effect where
    DispatchEvent :: Typeable a => a -> HsPluginE es m ()
    SubscribeEvent :: Typeable a => (a -> Eff (HsPluginE es : es) ()) -> HsPluginE es m ()
    GetState :: (PluginState a,Typeable a) => Proxy a -> HsPluginE es m a
    PutState :: (PluginState a,Typeable a) => a -> HsPluginE es m ()

makeEffect ''HsPluginE

getState' :: (HsPluginE as :> es,Typeable a,PluginState a) => Eff es a
getState' = getState Proxy

interpretHsPlugin :: forall es a. -- State (HsPluginState (Eff (HsPluginE : es))) :> es =>
            HsPluginState (Eff (HsPluginE es : es))
         -> Eff (HsPluginE es : es) a
         -> Eff es (a,HsPluginState (Eff (HsPluginE es : es)))
interpretHsPlugin s = reinterpret interpretState (const toState)
  where interpretState = runState s

        toState :: forall b l. HsPluginE es (Eff l) b ->
                               Eff (State (HsPluginState (Eff (HsPluginE es : es))) : es) b
        toState (DispatchEvent event) =
          do ls <- gets listeners
             forM_ (getListenersValues ls event) $ \l -> conv l
        toState (SubscribeEvent handler') =
          modify $ \s' -> s' {listeners = addListener Proxy handler' $ listeners s'}
        toState (GetState proxy') =
          do mv <- gets (getValueOfType proxy' . pluginStates)
             case mv of
               Just v -> pure v
               Nothing ->
                 do v <- conv initializeState
                    modify $ \s' -> s'
                       {pluginStates = setValueInMap (pluginStates s) v}
                    pure v
        toState (PutState v) =
          modify $ \s' -> s' {pluginStates = setValueInMap (pluginStates s) v}


        conv l = stateM (\s' -> raise (interpretHsPlugin s' l))



-- runHsPlugin :: Eff (HsPluginE:es) a -> HsPluginState (Eff (HsPluginE:es)) -> Eff es (a,HsPluginState (Eff (HsPluginE:es)))
-- runHsPlugin eff state = runState state $ interpretHsPlugin eff

emptyState :: HsPluginState (Eff (HsPluginE es : es))
emptyState = HsPluginState (TypeMap M.empty) (ListenerMap M.empty)

initialize :: Eff (HsPluginE es : es) a
           -> Eff es (a,HsPluginState (Eff (HsPluginE es : es)))
initialize = interpretHsPlugin emptyState

initializeWithListeners :: ListenerMap (Eff (HsPluginE es : es) ())
                        -> Eff (HsPluginE es : es) a
                        -> Eff es (a,HsPluginState (Eff (HsPluginE es : es)))
initializeWithListeners hooks = interpretHsPlugin $ HsPluginState (TypeMap M.empty) hooks

initializeWithPlugins :: [Eff (HsPluginE es : es) ()]
                      -> Eff (HsPluginE es : es) a
                      -> Eff es (a,HsPluginState (Eff (HsPluginE es : es)))
initializeWithPlugins plugs runner = initialize $
  do sequence_ plugs
     runner

