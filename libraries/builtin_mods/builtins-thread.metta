
;!(in-module sys::thread)
;; this is loaded as !(import &self thread)

; This adds thread-related types to the type system
(: ThreadHandle Type)

; The system-level get-type is nondeterministic, therefore returns all willing participants
(= (get-type $obj)
   (call-for! ThreadHandle
	(, 
	 (atomic $obj)
     (thread_property $th $_) ; iterates each thread handle
	 (== $th $obj))))

; ---- Transactions and Snapshots ----


(@doc thread:snapshot!
  (@desc "Creates a snapshot of an evaluation query.")
  (@params (
	(@param "List (Atom)")))
  (@return "Snapshot result (%Undefined%)"))
(: thread:snapshot! (-> Atom %Undefined%))
(= (thread:snapshot! $expr) (snapshot $expr))

(@doc thread:transaction!
  (@desc "Runs a transaction on an evaluation query.")
  (@params (
	(@param "List (Atom)")))
  (@return "Transaction result (%Undefined%)"))
(: thread:transaction! (-> Atom %Undefined%))
(= (thread:transaction! $expr) (transaction $expr))

(@doc thread:current-transaction!
  (@desc "Retrieves the current snapshot or transaction.")
  (@params ())
  (@return "Transaction (%Undefined%)"))
(: thread:current-transaction! (-> %Undefined%))
(= (thread:current-transaction!) (call-fn! current_transaction))

(@doc thread:transaction-updates!
  (@desc "Retrieves updates for a transaction.")
  (@params ())
  (@return "Updates (%Undefined%)"))
(: thread:transaction-updates! (-> %Undefined%))
(= (thread:transaction-updates!) (call-fn! transaction_updates))

(@doc thread:show-transaction!
  (@desc "Displays the current transaction and updates.")
  (@params ())
  (@return "Unit atom or Error"))
(: thread:show-transaction! (-> (->)))
(= (thread:show-transaction!)
   (call-unit! (@
	 (current_transaction $tx)
	 (format 'Current transaction: ~w~n' [$tx])
	 (transaction_updates $updates)
	 (format 'Pending updates: ~w~n' [$updates]))))

; ---- Core Async and Parallel Execution ----

(@doc thread:spawn-lazy!
  (@desc "Spawns a lazy thread to evaluate a query.")
  (@params (
	(@param "List (Atom)")))
  (@return "Thread handle (ThreadHandle)"))
(: thread:spawn-lazy! (-> Atom ThreadHandle))
(= (thread:spawn-lazy! $eval $handle)
   (call-for! $handle
	 (lazy_thread $eval $handle)))


(@doc thread:async!
  (@desc "Starts an asynchronous operation with a variable/value as the result.")
  (@params (
	(@param "Expression to evaluate (Atom)")))
  (@return "Value (Atom)"))
(: thread:async! (-> Atom Atom))
(= (thread:async! $eval) (call-fn! async_eval $eval))


(@doc thread:async-token!
  (@desc "Starts an asynchronous operation with a token as result.")
  (@params (
	(@param "Expression to evaluate (Atom)")))
  (@return "Token (Token)"))
(: thread:async-token! (-> Atom Variable Token))
(= (thread:async-token! $eval $result)
   (call-fn async_eval_token $eval $result))

(@doc thread:async-policy!
  (@desc "Starts an asynchronous operation with a policy and token as result.")
  (@params (
	(@param "List (Atom)")
	(@param "Policy (Atom)")))
  (@return "Token (Token)"))
(: thread:async-policy! (-> Atom Atom Token))
(= (thread:async-policy! $eval $policy $token)
   (call-for $token
	 (async_thread_with_policy $eval $token $policy)))

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
    (@param "Goal (Atom)")))
  (@return "Thread handle (ThreadHandle)"))
(: thread:spawn! (-> Atom ThreadHandle))
(= (thread:spawn! $eval)
   (call-for! $handle
     (thread_create $eval $handle ())))

(@doc thread:await!
  (@desc "Waits for the specified thread handle and retrieves the result.")
  (@params (
    (@param "Thread handle (ThreadHandle)")))
  (@return "Result (%Undefined%)"))
(: thread:await! (-> ThreadHandle %Undefined%))
(= (thread:await! $handle)
   (call-for! $result
     (thread_join $handle $result)))


(@doc thread:timeout!
  (@desc "Runs a thread with a specific timeout and retrieves the result.")
  (@params (
    (@param "Thread handle (ThreadHandle)")
    (@param "Timeout duration (Number)")))
  (@return "Result (%Undefined%)"))
(: thread:timeout! (-> ThreadHandle Number %Undefined%))
(= (thread:timeout! $handle $timeout)
   (call-for! $result
     (thread:cancel-after! $handle $timeout $result)))

(@doc thread:completed?
  (@desc "Checks if the thread has completed execution.")
  (@params (
    (@param "Thread handle (ThreadHandle)")))
  (@return "Boolean"))
(: thread:completed? (-> ThreadHandle Bool))
(= (thread:completed? $handle)
    (thread:is-complete $handle))


(@doc thread:limit-results!
  (@desc "Limits the number of results from a query.")
  (@params (
    (@param "Limit (Number)")
    (@param "List (Atom)")))
  (@return "Results or Error"))
(: thread:limit-results! (-> Number Atom Atom))
(= (thread:limit-results! $limit $expr)
   (call-for! $result (limit $limit (eval $expr $result))))

; ---- Parallel Group Control ----

(@doc thread:hyperpose!
  (@desc "Runs several threads in parallel, collecting a list of results.")
  (@params (
    (@param "List of Atoms to evaluate (Expression)")))
  (@return "List of results (Expression)"))
(: thread:hyperpose! (-> Expression Expression))
(= (thread:hyperpose! $exprs)
   (hyperpose $exprs))

(@doc thread:race!
  (@desc "Runs several threads in parallel and returns the first result.")
  (@params (
    (@param "List of Atoms to evaluate (Expression)")))
  (@return "First result (Atom)"))
(: thread:race! (-> Expression Atom))
(= (thread:race! $exprs)
   (call-for! $result
     (thread_race $exprs $result)))

(@doc thread:limit-time!
  (@desc "Runs a thread with a timed limit.")
  (@params (
    (@param "Timeout duration (Number)")
    (@param "List (Atom)")))
  (@return "Result (%Undefined%)"))
(: thread:limit-time! (-> Number Atom %Undefined%))
(= (thread:limit-time! $seconds $expr)
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
   (call-unit!
	 (thread_create
	   ((thread:sleep! $timeout) (thread:cancel! $handle))
	   $_ [(detached true)])))

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
  (@return "List of threads (Expression)"))
(: thread:list! (-> Unit Expression))
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
	(@param "List to be evaluated in the thread")
	(@param "Options Expression (TODO)")))
  (@return "Thread handle or error if thread creation fails"))
(: thread:create-with-options! (-> Atom Expression ThreadHandle))
(= (thread:create-with-options! $eval $opt)
   (call-for! $id
	 (thread_create $eval $id $opt)))

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
  (@desc "Returns True if the given term is a reference to or alias name of an existing engine.")
  (@params ((@param "Object to check")))
  (@return "Boolean"))
(: engine:is-engine? (-> %Undefined% Bool))
(= (engine:is-engine? $obj)
   (call-p! is_engine $obj))

(@doc engine:create
  (@desc "Creates a new engine to evaluate the goal, binding results to Template. Options may include alias, stack, etc.")
  (@params ((@param "Template: Variable or pattern to bind result")
  (@param "Goal: Expression to evaluate")
  (@param "Options: Expression of (key value) pairs")))
  (@return "Engine handle"))
(: engine:create (-> %Undefined% Atom Expression EngineHandle))
(= (engine:create $eval $options)
   (call-for! $handle (@ engine_create $template (@ eval_args $eval $template) $handle $options)))

(@doc engine:next
  (@desc "Requests the engine to produce the next answer. Starts the engine or resumes after a yield.")
  (@params ((@param "Engine handle (EngineHandle)")))
  (@return "Bound result or failure (%Undefined%)"))
(: engine:next (-> EngineHandle %Undefined%))
(= (engine:next $handle)
   (call-fn! engine_next $handle))

(@doc engine:next-reified
  (@desc "Returns the next result in reified form: the(Answer), no, or throw(Exception).")
  (@params ((@param "Engine handle (EngineHandle)")))
  (@return "Reified result (%Undefined%)"))
(: engine:next-reified (-> EngineHandle %Undefined%))
(= (engine:next-reified $handle)
   (call-fn! engine_next_reified $handle)) 

(@doc engine:destroy
  (@desc "Destroys the engine and frees associated resources.")
  (@params ((@param "Engine handle (EngineHandle)")))
  (@return "Unit atom or Error"))
(: engine:destroy (-> EngineHandle (->)))
(= (engine:destroy $handle)
   (call-unit! (engine_destroy $handle)))

(@doc engine:post
  (@desc "Posts a term into the engine to be consumed via engine:fetch. Must be followed by engine:next.")
  (@params ((@param "Engine handle (EngineHandle)") (@param "Term to send (Atom)")))
  (@return "Unit atom or Error"))
(: engine:post (-> EngineHandle Atom (->)))
(= (engine:post $handle $term)
   (call-unit! (engine_post $handle $term)))

(@doc engine:yield
  (@desc "From inside an engine: yield a value to the engine:next caller. Execution resumes on next.")
  (@params ((@param "Value to yield (%Undefined%)")))
  (@return "Unit atom or Error"))
(: engine:yield (-> %Undefined% (->)))
(= (engine:yield $value)
   (call-unit! (engine_yield $value)))

(@doc engine:fetch
  (@desc "From within the engine: receives a value posted from outside via engine:post. Fails if none exists.")
  (@params ())
  (@return "Posted value (%Undefined%)"))
(: engine:fetch (-> %Undefined%))
(= (engine:fetch) (call-fn! engine_fetch))

(@doc engine:self
  (@desc "From within the engine: gets the handle to the current engine.")
  (@params ())
  (@return "Current engine handle (EngineHandle)"))
(: engine:self (-> EngineHandle))
(= (engine:self) (call-fn! engine_self))

(@doc engine:property
  (@desc "Gets a property of the engine, such as status, alias, parent, etc.")
  (@params ((@param "Engine handle (EngineHandle)")))
  (@return "Engine property (%Undefined%)"))
(: engine:property (-> EngineHandle %Undefined%))
(= (engine:property $handle)
   (call-for! $prop (engine_property $handle $prop)))

(@doc engine:current-engine
  (@desc "Gets a currently known engine (one per invocation). Can be used to enumerate engines.")
  (@params ())
  (@return "Engine handle (EngineHandle)"))
(: engine:current-engine (-> EngineHandle))
(= (engine:current-engine)
   (call-fn! current_engine))

;; ----------------------------------------
;; Quick Engine Demonstration
;; ----------------------------------------

(@doc engine:quick-tests
  (@desc "Creates an engine to evaluate (member $X (1 2 3)), prints results, then destroys the engine.")
  (@params ())
  (@return "Unit atom or side effects"))
(: engine-quick-tests (->))
(= (engine-quick-tests)
   (let
      $E (engine:create (superpose (1 2 3)))
	(do
       (println! ("Result 1: " (engine:next $E)))
       (println! ("Result 2: " (engine:next $E)))
       (println! ("Result 3: " (engine:next $E)))

       (println! ("Reified result 4: " (engine:next-reified $E)))

     (engine:destroy $E)
       (println! "Engine test complete."))))
