module Semaphores where

import Env 
import Queue
import Scheduler

newMutex :: Store -> (Mutex, Store)
newMutex store =
  let (b,store') = newref store (BoolVal False)
      (q,store'') = newref store' (QueueVal emptyQueue)
  in  (Mutex b q, store'')

waitForMutex :: Mutex -> Thread -> Store -> SchedState -> (ExpVal, Store)
waitForMutex mutex thread store sched =
  let Mutex ref_to_closed ref_to_wait_queue = mutex
      closed = deref store ref_to_closed
      b = expValBool closed

      -- Then
      wait_queue = deref store ref_to_wait_queue
      q = expValQueue wait_queue
      q' = enqueue q thread
      qval = QueueVal q'
      then_store' = setref store ref_to_wait_queue qval

      -- Else
      else_store' = setref store ref_to_closed (BoolVal True)
  in
    if b
    then runNextThread then_store' sched
    else thread else_store' sched

signalMutex :: Mutex -> Thread -> Store -> SchedState -> (ExpVal, Store)
signalMutex mutex thread store sched = 
  let Mutex ref_to_closed ref_to_wait_queue = mutex
      closed = deref store ref_to_closed 
      b = expValBool closed
      
      wait_queue = deref store ref_to_wait_queue 
      q = expValQueue wait_queue

  in if b
     then if isEmpty q
             then let store' = setref store ref_to_closed (BoolVal False)
                  in  thread store' sched
             else dequeueWithFun q
                    (\first_waiting_thread other_waiting_threads store1 sched1 ->
                       let sched1' = placeOnReadyQueue first_waiting_thread sched1
                           store1' = setref store1 ref_to_wait_queue
                                        (QueueVal other_waiting_threads)
                       in thread store1' sched1') store sched
     else thread store sched
          
