{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-|
Module: Supervisors
Description: Montior a pool of threads.

This module exposes a 'Supervisor' construct, which can be used to safely
spawn threads while guaranteeing that:

* When the supervisor is killed, all of the threads it supervises will be
  killed.
* Child threads can terminate in any order, and memory usage will always
  be proportional to the number of *live* supervised threads.
-}
module Supervisors
    ( Supervisor
    , withSupervisor
    , supervise
    , superviseSTM
    ) where

import Control.Concurrent.STM

import Control.Concurrent       (ThreadId, forkIO, myThreadId, throwTo)
import Control.Concurrent.Async (withAsync)
import Control.Exception.Safe
    ( Exception
    , SomeException
    , bracket
    , bracket_
    , finally
    , toException
    , withException
    )
import Control.Monad            (forever, void)

import qualified Data.Set as S

-- | A handle for a supervisor, which montiors a pool of threads.
data Supervisor = Supervisor
    { stateVar :: TVar (Either SomeException (S.Set ThreadId))
    , runQ     :: TQueue (IO ())
    }

-- | Start a new supervisor, and return it.
newSupervisor :: IO Supervisor
newSupervisor = do
    stateVar <- newTVarIO $ Right S.empty
    runQ <- newTQueueIO
    let sup = Supervisor
            { stateVar = stateVar
            , runQ = runQ
            }
    pure sup

-- | Run the logic associated with the supervisor. This never returns until
-- the supervisor receives an (asynchronous) exception. When it does return,
-- all of the supervised threads will be killed.
runSupervisor :: Supervisor -> IO ()
runSupervisor sup@Supervisor{runQ=q} =
    forever (atomically (readTQueue q) >>= supervise sup)
    `withException`
    \e -> throwKids sup (e :: SomeException)

-- | Run an IO action with access to a supervisor. Threads spawned using the
-- supervisor will be killed when the action returns.
withSupervisor :: (Supervisor -> IO a) -> IO a
withSupervisor f = do
    sup <- newSupervisor
    withAsync (runSupervisor sup) $ const (f sup)

-- | Throw an exception to all of a supervisor's children, using 'throwTo'.
throwKids :: Exception e => Supervisor -> e -> IO ()
throwKids Supervisor{stateVar=stateVar} exn =
    bracket
        (atomically $ readTVar stateVar >>= \case
            Left _ ->
                pure S.empty
            Right kids -> do
                writeTVar stateVar $ Left (toException exn)
                pure kids)
        (foldr
            -- important: chain these together with `finally`,
            -- rather than (>>=) or friends, so that if one
            -- throws an exception we still run the others.
            (\kid old -> throwTo kid exn `finally` old)
            (pure ())
        )
        (\_ -> pure ())

-- | Launch the IO action in a thread, monitored by the 'Supervisor'. If the
-- supervisor receives an exception, the exception will also be raised in the
-- child thread.
supervise :: Supervisor -> IO () -> IO ()
supervise Supervisor{stateVar=stateVar} task =
    void $ forkIO $ bracket_ addMe removeMe task
  where
    -- | Add our ThreadId to the supervisor.
    addMe = do
        me <- myThreadId
        atomically $ do
            supState <- readTVar stateVar
            case supState of
                Left e ->
                    throwSTM e
                Right kids -> do
                    let !newKids = S.insert me kids
                    writeTVar stateVar $ Right newKids
    -- | Remove our ThreadId from the supervisor, so we don't leak it.
    removeMe = do
        me <- myThreadId
        atomically $ modifyTVar' stateVar $ \case
            state@(Left _) ->
                -- The supervisor is already stopped; we don't need to
                -- do anything.
                state
            Right kids ->
                -- We need to remove ourselves from the list of children;
                -- if we don't, we'll leak our ThreadId until the supervisor
                -- exits.
                --
                -- The use of $! here is very important, because even though
                -- modifyTVar' is strict, it only does whnf, so it would leave
                -- the state only evaluated as far as @Right (S.delete me kids)@;
                -- in that case we would still leak @me@.
                Right $! S.delete me kids

-- | Like 'supervise', but can be used from inside 'STM'. The thread will be
-- spawned if and only if the transaction commits.
superviseSTM :: Supervisor -> IO () -> STM ()
superviseSTM Supervisor{runQ=q} = writeTQueue q
