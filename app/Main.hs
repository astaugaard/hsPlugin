{-# LANGUAGE TemplateHaskell, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import HsPlugin
import Data.Typeable
import Effectful
import Control.Monad (forM_, void)

data TestEvent = TestEvent deriving (Typeable)

newtype TestCounter = TestCounter Int deriving (Typeable)

newtype TestStartingValue = TestStartingValue Int deriving (Typeable)

instance PluginState TestCounter where
    initializeState = do (TestStartingValue a) <- getState'
                         pure (TestCounter a)

instance PluginState TestStartingValue where
    initializeState = pure $ TestStartingValue 0

main' :: Eff [HsPluginE '[IOE],IOE] ()
main' = forM_ [1..10] $ const $ dispatchEvent TestEvent

counterHandler :: Listener (Eff '[HsPluginE '[IOE],IOE] ())
counterHandler = Listener (\TestEvent ->
  do (TestCounter i) <- getState'
     liftIO $ print i
     putState (TestCounter (i+1))
     ) Proxy

main :: IO ()
main = void $ runEff $ initializeWithListeners
   (makeListenerMap [counterHandler]) main'
