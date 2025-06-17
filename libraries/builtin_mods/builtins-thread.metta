


!(in-module sys::thread)
;; this is loaded as !(import &self thread)

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
  (@return "Snapshot result (%Undefined%)"))
(: thread:snapshot! (-> Atom %Undefined%))
(= (thread:snapshot! $expr $result)
   (call-for! $result
	 (snapshot (eval_args $expr $result))))

(@doc thread:transaction!
  (@desc "Runs a transaction on an evaluation query.")
  (@params (
	(@param "Expression (Atom)")))
  (@return "Transaction result (%Undefined%)"))
(: thread:transaction! (-> Atom %Undefined%))
(= (thread:transaction! $expr $result)
   (call-for! $result
	 (transaction (eval_args $expr $result))))

(@doc thread:current-transaction!
  (@desc "Retrieves the current snapshot or transaction.")
  (@params ())
  (@return "Transaction (%Undefined%)"))
(: thread:current-transaction! (-> %Undefined%))
(= (thread:current-transaction! $tx)
   (call-for! $tx (current_transaction $tx)))

(@doc thread:transaction-updates!
  (@desc "Retrieves updates for a transaction.")
  (@params ())
  (@return "Updates (%Undefined%)"))
(: thread:transaction-updates! (-> %Undefined%))
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
  (@return "Result (%Undefined%)"))
(: thread:await! (-> ThreadHandle %Undefined%))
(= (thread:await! $handle $result)
   (call-for! $result
     (thread_join $handle $result)))


(@doc thread:timeout!
  (@desc "Runs a thread with a specific timeout and retrieves the result.")
  (@params (
    (@param "Thread handle (ThreadHandle)")
    (@param "Timeout duration (Number)")))
  (@return "Result (%Undefined%)"))
(: thread:timeout! (-> ThreadHandle Number %Undefined%))
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
  (@return "Result (%Undefined%)"))
(: thread:limit-time! (-> Number Atom %Undefined%))
(= (thread:limit-time! $seconds $expr $result)
   (call-for! $result
     (cwtl $seconds (eval_args $expr $result))))

; ---- Internal thread handling and operations ----

(@doc thread:cancel-after!
  (@desc "Creates a timer thread that after a durration kills a peer.")
  (@params (
    (@param "Thread handle (ThreadHandle)")
    (@param "Timeout (Number)")))
  (@return "Result (%Undefined%)"))
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
    (@param "Query (%Undefined%)")))
  (@return "Unit atom or Error"))
(: thread:mutex-with! (-> Atom %Undefined% (->)))
(= (thread:mutex-with! $mutex-symbol $query)
   (call-unit! (with_mutex $mutex-symbol $query)))

; ---- Message Queues ----


(: MessageQueue Type)

;; Type detection for Message Queues
(= (get-type $obj)
   (call-for! MessageQueue
     (atomic $obj)
     (message_queue_property $queue $_)
	 (== $queue $obj)))

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
    (@param "Message (%Undefined%)")))
  (@return "Unit atom or Error"))
(: thread:send-message! (-> Atom %Undefined% (->)))
(= (thread:send-message! $queue $msg)
   (call-unit! (thread_send_message $queue $msg)))

(@doc thread:receive-message!
  (@desc "Receives a message from a queue.")
  (@params (
    (@param "Queue (Atom)")))
  (@return "Message (%Undefined%)"))
(: thread:receive-message! (-> Atom %Undefined%))
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



;; ----------------------------------------
;; MeTTa Engine API 
;; ----------------------------------------

(: EngineHandle Type)

;; Type detection for engine handles
(= (get-type $obj)
   (call-for! EngineHandle
     (atomic $obj)
     (is_engine? $obj)))

;; Check if a term is a valid engine handle or alias
(@doc engine:is-engine?
  (@desc "Returns True if the given term is a reference to or alias name of an existing engine."))
(: engine:is-engine? (-> %Undefined% Bool))
(= (engine:is-engine? $obj)
   (call-p! is_engine $obj))

;; Create an engine from a Template, Goal, and Options
(@doc engine:create
  (@desc "Creates a new engine to evaluate the goal, binding results to Template. Options may include alias, stack, etc."))
(@params (
  (@param "Template: Variable or pattern to bind result")
  (@param "Goal: Expression to evaluate")
  (@param "Options: List of (key value) pairs")))
(@return "Engine handle")
(: engine:create (-> %Undefined% Atom List EngineHandle))
(= (engine:create $template $goal $options)
   (call-for! $handle (engine_create $template $goal $handle $options)))

;; engine_next/2 – produce the next answer
(@doc engine:next
  (@desc "Requests the engine to produce the next answer. Starts the engine or resumes after a yield. Returns bound template or fails."))
(: engine:next (-> EngineHandle %Undefined%))
(= (engine:next $handle)
   (call-for! $result (engine_next $handle $result)))

;; engine_next_reified/2 – returns structured status: the(Result), no, throw(Exception)
(@doc engine:next-reified
  (@desc "Returns the next result in reified form: the(Answer), no, or throw(Exception)."))
(: engine:next-reified (-> EngineHandle %Undefined%))
(= (engine:next-reified $handle)
   (call-for! $result (engine_next_reified $handle $result)))

;; engine_destroy/1 – clean up engine and resources
(@doc engine:destroy
  (@desc "Destroys the engine and frees associated resources."))
(: engine:destroy (-> EngineHandle (->)))
(= (engine:destroy $handle)
   (call-unit! (engine_destroy $handle)))

;; engine_post/2 – send a term to be received by engine_fetch/1
(@doc engine:post
  (@desc "Posts a term into the engine to be consumed via engine:fetch. Must be followed by engine:next."))
(: engine:post (-> EngineHandle Atom (->)))
(= (engine:post $handle $term)
   (call-unit! (engine_post $handle $term)))

;; engine_yield/1 – pause engine and yield value to caller
(@doc engine:yield
  (@desc "From inside an engine: yield a value to the engine:next caller. Execution resumes on next."))
(: engine:yield (-> %Undefined% (->)))
(= (engine:yield $value)
   (call-unit! (engine_yield $value)))

;; engine_fetch/1 – from inside an engine, receive a posted value
(@doc engine:fetch
  (@desc "From within the engine: receives a value posted from outside via engine:post. Fails if none exists."))
(: engine:fetch (-> %Undefined%))
(= (engine:fetch)
   (call-for! $value (engine_fetch $value)))

;; engine_self/1 – from within an engine, get a handle to self
(@doc engine:self
  (@desc "From within the engine: gets the handle to the current engine."))
(: engine:self (-> EngineHandle))
(= (engine:self)
   (call-for! $handle (engine_self $handle)))

;; engine_property/2 – query engine metadata
(@doc engine:property
  (@desc "Gets a property of the engine, such as status, alias, parent, etc."))
(: engine:property (-> EngineHandle %Undefined%))
(= (engine:property $handle)
   (call-for! $prop (engine_property $handle $prop)))

;; current_engine/1 – iterate all current engine handles
(@doc engine:current-engine
  (@desc "Gets a currently known engine (one per invocation). Can be used to enumerate engines."))
(: engine:current-engine (-> EngineHandle))
(= (engine:current-engine)
   (call-for! $handle (current_engine $handle)))

;; ----------------------------------------
;; Quick Engine Demonstration
;; ----------------------------------------

(@doc engine-quick-test
  (@desc "Creates an engine to evaluate (member $X (1 2 3)), prints results, then destroys the engine."))
(: engine-quick-test (->))
(= (engine-quick-test)
   (let
      $E (engine:create $X (member $X (1 2 3)) ())
	(do
     (print "Result 1: " (engine:next $E))
     (print "Result 2: " (engine:next $E))
     (print "Result 3: " (engine:next $E))

     (print "Reified result 4: " (engine:next-reified $E))

     (engine:destroy $E)
     (print "Engine test complete.")))

