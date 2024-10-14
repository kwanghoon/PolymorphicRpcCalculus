module Scheduler where

import Env
import Queue
import Data.Maybe

--
timeslice = 20

--
initializeScheduler :: Integer -> SchedState
initializeScheduler ticks =
  SchedState {
   theReadyQueue = emptyQueue,
   theFinalAnswer = Nothing,
   theMaxTimeSlice = ticks,
   theTimeRemaining = ticks
  }

placeOnReadyQueue :: Thread -> SchedState -> SchedState
placeOnReadyQueue th scState =
  scState { theReadyQueue = enqueue (theReadyQueue scState) th }

runNextThread :: Store -> SchedState -> (ExpVal, Store)
runNextThread store scState =
  if isEmpty (theReadyQueue scState)
  then (fromJust (theFinalAnswer scState), store)
  else
    dequeueWithFun (theReadyQueue scState)
     (\first_ready_thread other_ready_threads ->
        first_ready_thread
          store
          ( scState { theReadyQueue = other_ready_threads,
                      theTimeRemaining = theMaxTimeSlice scState } ) )

setFinalAnswer :: SchedState -> ExpVal -> SchedState
setFinalAnswer scState val = scState { theFinalAnswer = Just val }

timeExpired :: SchedState -> Bool
timeExpired scState = theTimeRemaining scState==0

decrementTimer :: SchedState -> SchedState
decrementTimer scState = scState { theTimeRemaining = theTimeRemaining scState - 1 }
