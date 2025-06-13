;; this is loaded as !(import &self threads)

; This adds thread-related types to the type system
(: ThreadHandle Type)

; The system-level get-type is nondeterministic, therefore returns all willing participants
(= (get-type $obj)
   (call-for! ThreadHandle
     (atomic $obj)
     (thread_property $p $_) ; iterates each thread handle
	 (== $p $obj)))

; ---- Transactions and Snapshots ----

(@doc thread:snapshot!
  (@desc "Creates a snapshot of an evaluation query.")
  (@params (
	(@param "Expression (Atom)")))
  (@return "Snapshot result (Any)"))
(: thread:snapshot! (-> Atom Any))
(= (thread:snapshot! $expr $result)
   (call-for! $result
	 (snapshot (eval_args $expr $result))))

(@doc thread:transaction!
  (@desc "Runs a transaction on an evaluation query.")
  (@params (
	(@param "Expression (Atom)")))
  (@return "Transaction result (Any)"))
(: thread:transaction! (-> Atom Any))
(= (thread:transaction! $expr $result)
   (call-for! $result
	 (transaction (eval_args $expr $result))))

(@doc thread:current-transaction!
  (@desc "Retrieves the current snapshot or transaction.")
  (@params ())
  (@return "Transaction (Any)"))
(: thread:current-transaction! (-> Any))
(= (thread:current-transaction! $tx)
   (call-for! $tx (current_transaction $tx)))

(@doc thread:transaction-updates!
  (@desc "Retrieves updates for a transaction.")
  (@params ())
  (@return "Updates (Any)"))
(: thread:transaction-updates! (-> Any))
(= (thread:transaction-updates! $updates)
   (call-unit! (transaction_updates $updates)))

(@doc thread:show-transaction!
  (@desc "Displays the current transaction and updates.")
  (@params ())
  (@return "Unit atom or Error"))
(: thread:show-transaction! (-> (->)))
(= (thread:show-transaction!)
   (call-unit! ((
	 (current_transaction $tx)
	 (format 'Current transaction: ~w~n' [$tx])
	 (transaction_updates $updates)
	 (format 'Pending updates: ~w~n' [$updates])))))

; ---- Core Async and Parallel Execution ----

(@doc thread:spawn-lazy!
  (@desc "Spawns a lazy thread to evaluate a query.")
  (@params (
	(@param "Expression (Atom)")))
  (@return "Thread handle (ThreadHandle)"))
(: thread:spawn-lazy! (-> Atom ThreadHandle))
(= (thread:spawn-lazy! $goal $handle)
   (call-for! $handle
	 (lazy_thread $goal $handle)))

(@doc thread:async!
  (@desc "Starts an asynchronous operation with a token as result.")
  (@params (
	(@param "Expression (Atom)")))
  (@return "Token (Token)"))
(: thread:async! (-> Atom Token))
(= (thread:async! $goal $token)
   (call-for $token
	 (async_thread $goal $token)))

(@doc thread:async-policy!
  (@desc "Starts an asynchronous operation with a policy and token as result.")
  (@params (
	(@param "Expression (Atom)")
	(@param "Policy (Atom)")))
  (@return "Token (Token)"))
(: thread:async-policy! (-> Atom Atom Token))
(= (thread:async-policy! $goal $policy $token)
   (call-for $token
	 (async_thread_with_policy $goal $token $policy)))

(@doc thread:await-token!
  (@desc "Waits for an asynchronous result from a token.")
  (@params (
	(@param "Token (Token)")))
  (@return "Unit atom or Error"))
(: thread:await-token! (-> Token (->)))
(= (thread:await-token! $token)
   (call-unit! (await_token $token)))

; ---- Thread Management and Execution ----

(@doc thread:spawn!
  (@desc "Spawns a new thread to execute the provided goal.")
  (@params (
    (@param "Goal (Atom)"))
  (@return "Thread handle (ThreadHandle)"))
(: thread:spawn! (-> Atom ThreadHandle))
(= (thread:spawn! $goal $handle)
   (call-for! $handle
     (thread_create $goal $handle [])))

(@doc thread:await!
  (@desc "Waits for the specified thread handle and retrieves the result.")
  (@params (
    (@param "Thread handle (ThreadHandle)")))
  (@return "Result (Any)"))
(: thread:await! (-> ThreadHandle Any))
(= (thread:await! $handle $result)
   (call-for! $result
     (thread_join $handle $result)))


(@doc thread:timeout!
  (@desc "Runs a thread with a specific timeout and retrieves the result.")
  (@params (
    (@param "Thread handle (ThreadHandle)")
    (@param "Timeout duration (Number)")))
  (@return "Result (Any)"))
(: thread:timeout! (-> ThreadHandle Number Any))
(= (thread:timeout! $handle $timeout $result)
   (call-for! $result
     (thread:cancel-after! $handle $timeout $result)))

(@doc thread:completed?
  (@desc "Checks if the thread has completed execution.")
  (@params (
    (@param "Thread handle (ThreadHandle)")))
  (@return "Boolean"))
(: thread:completed? (-> ThreadHandle Bool))
(= (thread:completed? $handle $completed)
   (call-for! $completed
     (thread:is-complete $handle)))


(@doc thread:limit-results!
  (@desc "Limits the number of results from a query.")
  (@params (
    (@param "Limit (Number)")
    (@param "Expression (Atom)")))
  (@return "Results or Error"))
(: thread:limit-results! (-> Number Atom Atom))
(= (thread:limit-results! $limit $expr)
   (call-for! $result (limit $limit (eval $expr $result))))

; ---- Parallel Group Control ----

(@doc thread:hyperpose!
  (@desc "Runs several threads in parallel, collecting a list of results.")
  (@params (
    (@param "List of expressions (List)")))
  (@return "List of results (List)"))
(: thread:hyperpose! (-> List List))
(= (thread:hyperpose! $exprs $results)
   (call-for! $results
     (metta_hyperpose $exprs $results)))

(@doc thread:race!
  (@desc "Runs several threads in parallel and returns the first result.")
  (@params (
    (@param "List of expressions (List)")))
  (@return "First result (Atom)"))
(: thread:race! (-> List Atom))
(= (thread:race! $exprs $result)
   (call-for! $result
     (thread_race $exprs $result)))

(@doc thread:limit-time!
  (@desc "Runs a thread with a timed limit.")
  (@params (
    (@param "Timeout duration (Number)")
    (@param "Expression (Atom)")))
  (@return "Result (Any)"))
(: thread:limit-time! (-> Number Atom Any))
(= (thread:limit-time! $seconds $expr $result)
   (call-for! $result
     (cwtl $seconds (eval_args $expr $result))))

; ---- Internal thread handling and operations ----

(@doc thread:cancel-after!
  (@desc "Creates a timer thread that after a durration kills a peer.")
  (@params (
    (@param "Thread handle (ThreadHandle)")
    (@param "Timeout (Number)")))
  (@return "Result (Any)"))
(: thread:cancel-after! (-> ThreadHandle Number (->)))
(= (thread:cancel-after! $handle $timeout)
	 (thread_create
	   ((thread:sleep! $timeout) (thread:cancel! $handle))
	   $_ [detached true]))

(@doc thread:is-complete
  (@desc "Checks if the thread has finished execution.")
  (@params (
    (@param "Thread handle (ThreadHandle)")))
  (@return "Boolean"))
(: thread:is-complete (-> ThreadHandle Bool))
(= (thread:is-complete $handle)
   (call-p! thread_property $handle (status completed)))

; ---- Error handling ----

(@doc thread:error!
  (@desc "Throws an error related to thread execution.")
  (@params (
    (@param "Thread handle (ThreadHandle)")
    (@param "Message (Atom)")))
  (@return "Unit atom or Error"))
(: thread:error! (-> ThreadHandle Atom (->)))
(= (thread:error! $handle $msg)
   (throw (error (thread_error $msg $handle))))

; ---- Mutex Primitives ----

(@doc thread:mutex-create!
  (@desc "Creates a mutex with a given symbol.")
  (@params (
    (@param "Mutex symbol (Atom)")))
  (@return "Unit atom or Error"))
(: thread:mutex-create! (-> Atom (->)))
(= (thread:mutex-create! $mutex-symbol)
   (call-unit! (mutex_create $mutex-symbol)))

(@doc thread:mutex-lock!
  (@desc "Locks a mutex with a given symbol.")
  (@params (
    (@param "Mutex symbol (Atom)")))
  (@return "Unit atom or Error"))
(: thread:mutex-lock! (-> Atom (->)))
(= (thread:mutex-lock! $mutex-symbol)
   (call-unit! (mutex_lock $mutex-symbol)))

(@doc thread:mutex-unlock!
  (@desc "Unlocks a mutex with a given symbol.")
  (@params (
    (@param "Mutex symbol (Atom)")))
  (@return "Unit atom or Error"))
(: thread:mutex-unlock! (-> Atom (->)))
(= (thread:mutex-unlock! $mutex-symbol)
   (call-unit! (mutex_unlock $mutex-symbol)))

(@doc thread:mutex-with!
  (@desc "Executes a query with a locked mutex.")
  (@params (
    (@param "Mutex symbol (Atom)")
    (@param "Query (Any)")))
  (@return "Unit atom or Error"))
(: thread:mutex-with! (-> Atom Any (->)))
(= (thread:mutex-with! $mutex-symbol $query)
   (call-unit! (with_mutex $mutex-symbol $query)))

; ---- Message Queues ----

(@doc thread:queue-create!
  (@desc "Creates a message queue.")
  (@params (
    (@param "Queue (Atom)")))
  (@return "Unit atom or Error"))
(: thread:queue-create! (-> Atom (->)))
(= (thread:queue-create! $queue)
   (call-unit! (message_queue_create $queue)))

(@doc thread:send-message!
  (@desc "Sends a message to a queue.")
  (@params (
    (@param "Queue (Atom)")
    (@param "Message (Any)")))
  (@return "Unit atom or Error"))
(: thread:send-message! (-> Atom Any (->)))
(= (thread:send-message! $queue $msg)
   (call-unit! (thread_send_message $queue $msg)))

(@doc thread:receive-message!
  (@desc "Receives a message from a queue.")
  (@params (
    (@param "Queue (Atom)")))
  (@return "Message (Any)"))
(: thread:receive-message! (-> Atom Any))
(= (thread:receive-message! $queue $msg)
   (call-unit! (thread_get_message $queue $msg)))

; ---- Thread Management ----

(@doc thread:set-priority!
  (@desc "Sets the priority level of the current thread.")
  (@params (
    (@param "Priority level (Integer)")))
  (@return "Unit atom or Error"))
(: thread:set-priority! (-> Number (->)))
(= (thread:set-priority! $level)
   (call-unit!
     (thread_self $id)
     (thread_set_priority $id $level)))

(@doc thread:self!
  (@desc "Retrieves the ID of the current thread.")
  (@params ())
  (@return "Thread ID (ThreadHandle)"))
(: thread:self! (-> Unit ThreadHandle))
(= (thread:self! $id)
   (call-for! $id
     (thread_self $id)))

(@doc thread:list!
  (@desc "Lists all threads with their properties.")
  (@params ())
  (@return "List of threads (List)"))
(: thread:list! (-> Unit List))
(= (thread:list! $threads)
   (call-for! $threads
     (findall (thread $id $status $eval-query)
      (, (thread_property $id status $status)
       (thread_property $id goal $eval-query)) $threads)))

(@doc thread:sleep!
  (@desc "Makes the current thread sleep for a given number of seconds.")
  (@params (
    (@param "Seconds (Number)")))
  (@return "Unit atom or Error"))
(: thread:sleep! (-> Number (->)))
(= (thread:sleep! $seconds)
   (call-unit! (sleep $seconds)))


(@doc thread:create-with-options!
  (@desc "Function takes Atom to evaluate and options (priority, detached, suspended etc) to create a thread and return a thread handle.")
  (@params (
	(@param "Expression to be evaluated in the thread")
	(@param "Options List (TODO)")))
  (@return "Thread handle or error if thread creation fails"))
(: thread:create-with-options! (-> Atom Expression ThreadHandle))
(= (thread:create-with-options! $goal $opt)
   (call-for! $id
	 (thread_create goal $id $opt)))

(@doc thread:join!
  (@desc "Function takes thread handle and joins the thread, waits for its completion.")
  (@params (
	(@param "Thread handle")))
  (@return "Unit atom or Error"))
(: thread:join! (-> ThreadHandle (->)))
(= (thread:join! $thread)
   (call-unit!
	 (thread_join $thread)))

(@doc thread:detach!
  (@desc "Function takes thread handle and detaches the thread, allowing it to run independently.")
  (@params (
	(@param "Thread handle")))
  (@return "Unit atom or Error"))
(: thread:detach! (-> ThreadHandle (->)))
(= (thread:detach! $thread)
   (call-unit!
	 (detach_thread $thread)))

(@doc thread:suspend!
  (@desc "Function takes thread handle and suspends the thread.")
  (@params (
	(@param "Thread handle")))
  (@return "Unit atom or Error"))
(: thread:suspend! (-> ThreadHandle (->)))
(= (thread:suspend! $thread)
   (call-unit!
	 (suspend_thread $thread)))

(@doc thread:resume!
  (@desc "Function takes thread handle and resumes the thread from suspension.")
  (@params (
	(@param "Thread handle")))
  (@return "Unit atom or Error"))
(: thread:resume! (-> ThreadHandle (->)))
(= (thread:resume! $thread)
   (call-unit!
	 (resume_thread $thread)))

(@doc thread:cancel!
  (@desc "Function takes thread handle and cancels the thread.")
  (@params (
	(@param "Thread handle")))
  (@return "Unit atom or Error"))
(: thread:cancel! (-> ThreadHandle (->)))
(= (thread:cancel! $thread)
   (call-unit!
	 (cancel_thread $thread)))

(@doc thread:status!
  (@desc "Function takes thread handle and returns the status of the thread.")
  (@params (
	(@param "Thread handle")))
  (@return "Thread status (running, suspended, completed)"))
(: thread:status! (-> ThreadHandle Atom))
(= (thread:status! $thread)
   (call-for! $status
	 (thread_property $thread (status $status))))

(@doc thread:priority!
  (@desc "Function takes thread handle and sets its priority.")
  (@params (
	(@param "Thread handle")
	(@param "Priority level (integer)")))
  (@return "Unit atom or Error"))
(: thread:priority! (-> ThreadHandle Number (->)))
(= (thread:priority! $thread $priority)
   (call-unit!
	 (set_thread_priority $thread $priority)))

