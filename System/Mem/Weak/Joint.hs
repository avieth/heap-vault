{-|
Module      : System.Mem.Weak.Joint
Description : Definition of JointWeak and Requisite.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

Create weak references which have a set of values which are jointly-sufficient,
individually-necessary for the reference to keep the value alive.
So long as the reference keeps the value alive, each requisite can be
dereferenced to obtain the value.
-}

module System.Mem.Weak.Joint (

      JointWeak
    , mkJointWeak
    , Requisite
    , mkRequisite
    , deRefRequisite

    ) where

import System.Mem
import System.Mem.Weak
import Data.IORef

data ControlState = Initial | Alive (IORef ()) | Dying

type Control = IORef ControlState

retain :: IORef () -> Control -> IO ()
retain = flip writeIORef . Alive

release :: Control -> IO ()
release = flip writeIORef Dying

-- | A reference to v which does not on its own keep v alive (it's like a
--   Weak v) but allows for the creation of 0 or more requisites for v:
--   things which are jointly sufficient to keep v alive. If any of
--   the requisites is collected (or if no requisites are created) then this
--   JointWeak drops its reference to v. Note: this does not mean that v
--   will necessarily be collected; there could be other references to it.
newtype JointWeak v = JointWeak {
      getJointWeak :: (Control, Weak (IORef ()), Weak v)
    }

{-# NOINLINE mkJointWeak #-}
mkJointWeak :: v -> IO (JointWeak v)
mkJointWeak v = do
    -- The proxy keeps the value alive.
    proxy <- newIORef ()
    wk <- mkWeak proxy v (Just (putStrLn "mkJointWeak : finalizing proxy"))
    -- Control references the proxy.
    -- Filling this with proxy will keep the value alive.
    -- Emptying this will cause its weak pointer to stop keeping the value
    -- alive.
    control <- newIORef Initial
    -- A weak reference to the proxy, so that we can expose it without keeping
    -- it alive. This allows us to fill control when a requisite is created.
    wkProxy <- mkWeakIORef proxy (pure ())
    pure (JointWeak (control, wkProxy, wk))

{-# NOINLINE deRefJointWeak #-}
deRefJointWeak :: JointWeak v -> IO (Maybe v)
deRefJointWeak = deRefWeak . (\(_,_,wk) -> wk) . getJointWeak

-- | A necessary condition for a JointWeak to hold on to its value. All of the
--   requisites created on a JointWeak are individually necessary, jointly
--   sufficient for the reference to remain.
newtype Requisite v = Requisite {
      getRequisite :: (IORef (), Weak v)
    }

-- | Make a requisite for a JointWeak v: the JointWeak will keep its value
--   alive as long as every requisite (i.e. this one included) is alive.
{-# NOINLINE mkRequisite #-}
mkRequisite :: JointWeak v -> IO (Requisite v)
mkRequisite (JointWeak (control, wkProxy, weak)) = do
    ref <- newIORef ()
    maybeProxy <- deRefWeak wkProxy
    case maybeProxy of
        Nothing -> do
            putStrLn "mkRequisite : JointWeak is already dead"
            pure (Requisite (ref, weak))
        Just proxy -> do
            controlState <- readIORef control
            case controlState of
                -- The JointWeak is new so we make sure that it's going
                -- to carry on living.
                Initial -> retain proxy control
                _ -> pure ()
            _ <- mkWeakIORef ref (putStrLn "mkRequisite : finalizing" >> release control)
            pure (Requisite (ref, weak))

-- | Dereference a requisite: just a deRefWeak.
deRefRequisite :: Requisite v -> IO (Maybe v)
deRefRequisite = deRefWeak . snd . getRequisite
