# MeTTa Threading & Execution Model (ISO-Prolog Enhanced)

## Table of Contents

* [Multithreading Constructs](#multithreading-constructs)
* [Object Isolation: `snapshot` vs `transaction`](#object-isolation-snapshot-vs-transaction)
* [Thread API](#thread-api-from-import-self-threads)
  * [Snapshot & Transaction](#-snapshot--transaction)
  * [Core Threading](#-core-threading)
  * [Execution Groups](#-execution-groups)
  * [Synchronization](#-synchronization)
  * [Message Queues](#-message-queues)
  * [Thread Lifecycle](#-thread-lifecycle)
  * [Error Handling](#-error-handling)
* [Threads vs Engines](#-threads-vs-engines)
* [What is a Template?](#-what-is-a-template)
* [Engine Options Format](#-engine-options-format)
* [Engine API](#-engine-api)

---

## Multithreading Constructs

| Construct      | Parallel? | Shared Vars? | Notes                                                              |
| -------------- | --------- | ------------ | ------------------------------------------------------------------ |
| `sequential`   | No        | Maybe        | Ordered execution, runtime analyzed                                |
| `parallel`     | Yes       | Maybe        | Parallel tasks, shared if safe                                     |
| `sequential-s` | No        | Yes          | Sequential with shared variables                                   |
| `parallel-s`   | Yes       | Yes          | Parallel with shared variables                                     |
| `sequential-i` | No        | No           | Explicit full isolation                                            |
| `parallel-i`   | Yes       | No           | Fully sandboxed parallel execution                                 |
| `isolated`     | Maybe     | No           | Chooses between `sequential-i` or `parallel-i` based on analysis   |
| `shared`       | Maybe     | Yes          | Chooses between `sequential-s` or `parallel-s` based on dependency |
| `progn`       | Maybe     | Maybe          | Chooses between `sequential-i/s` or `parallel-i/s` based on dependency |

### Construct Selection

`shared` and `isolated` automatically select safe, optimal execution strategies via runtime analysis:

- **`shared`** dispatches to `sequential-s` or `parallel-s` depending on data dependencies
- **`isolated`** dispatches to `sequential-i` or `parallel-i` when no dependency is detected

---

## Expression Rewriting

```metta
(likes Joe (friendsOf Mary))
````

Becomes:

```metta
(shared
  (likes Joe $mf)
  (equals $mf (friendsOf Mary)))
```

Or:

```metta
(parallel-s
  (likes Joe $mf)
  (friends Mary $mf))
```

---

## Execution Examples

Great — I’ll now rewrite the full **Execution Examples** section with that level of detail for each case. These explanations will describe:

* **Evaluation strategy** (`sequential`, `parallel`, etc.)
* **Variable sharing and flow**
* **Implications for correctness and performance**
* **Typical use cases**

---

## Execution Examples (Fully Explained)

---

### 1. Sequential: isolated then shared

```metta
(sequential
  (isolated (Task1 $x) (Task2 $y) (Task3 $x $y))
  (shared   (Task4 $x) (Task5 $y) (Task6 $x $y)))
```

**Behavior:**

* `sequential` enforces strict top-to-bottom execution.
* The first group is `isolated`, so all three `Task1`, `Task2`, and `Task3` run in a sandbox — they **do not see global bindings**.
* However, they share values between each other:

  * `Task1` binds `$x`
  * `Task2` binds `$y`
  * `Task3` uses both
* The second group is `shared`, meaning `$x` and `$y` must be available globally or from outside the block.

**Use case:** Complex state mutation followed by interpretation or reporting using shared values.

---

### 2. Parallel version of the same tasks

```metta
(parallel
  (isolated (Task1 $x) (Task2 $y) (Task3 $x $y))
  (shared   (Task4 $x) (Task5 $y) (Task6 $x $y)))
```

**Behavior:**

* All top-level sub-blocks (`isolated`, `shared`) run in parallel.
* Within the `isolated` group, tasks execute sequentially as before, but isolated.
* The `shared` block runs in parallel among its tasks.

**Effect:**

* Increases throughput if tasks are independent.
* Requires care: `Task4`–`Task6` may begin while `$x`/`$y` are still being resolved elsewhere unless managed carefully.

**Use case:** Data preparation (in `isolated`) concurrent with visualization or export (in `shared`).

---

### 3. Progn: strictly sequential mix of isolated/shared

```metta
(progn
  (isolated (Task1 $x) (Task2 $y) (Task3 $x $y))
  (shared   (Task4 $x) (Task5 $y) (Task6 $x $y)))
```

**Behavior:**

* `progn` is like `sequential`, but softer semantically — often used to enforce readable ordering.
* All expressions are guaranteed to run in order.
* Unlike `parallel`, nothing runs concurrently.

**Use case:** When both isolation and data reuse are needed, and correctness matters more than speed (e.g., in audit logging or interactive sessions).

---

### 4. Shared group combining internal parallel and sequential

```metta
(shared
  (parallel   (Task1 $x) (Task2 $y) (Task3 $x $y))
  (sequential (Task4 $x) (Task5 $y) (Task6 $x $y)))
```

**Behavior:**

* Outer `shared` means all sub-blocks can reuse and unify variables.
* Inside, the `parallel` group runs all three tasks at once.

  * `$x`, `$y` may be bound independently, assuming safe parallel access.
* The `sequential` block executes in order and can rely on previous bindings.

**Use case:** Running independent inferences in parallel, then serially applying them in a post-processing pipeline.

---

### 5. Isolated group with internal parallel + sequential logic

```metta
(isolated
  (parallel   (Task1 $x) (Task2 $y) (Task3 $x $y))
  (sequential (Task4 $x) (Task5 $y) (Task6 $x $y)))
```

**Behavior:**

* All sub-blocks execute inside a sandbox.
* `parallel` group runs first, computing `$x` and `$y` in isolation.
* Then `sequential` block consumes those isolated bindings in strict order.

**Effect:**

* No risk of corrupting global state.
* Result is a contained environment where `Task1`–`Task6` are run and discarded unless committed explicitly.

**Use case:** Transactional logic evaluation, such as test runs or inference previews.

---

### 6. Flattened `isolated` group with mixed inner forms

```metta
(isolated
  (parallel (Task1 $x) (Task2 $y) (Task3 $x $y))
  (Task4 $x)
  (Task5 $y)
  (Task6 $x $y))
```

**Behavior:**

* Everything is inside a single `isolated` block.
* `Task1`–`Task3` are computed concurrently.
* `Task4`–`Task6` execute sequentially afterward (implicitly ordered).

**Binding Flow:**

* `$x` and `$y` from `Task1` and `Task2` are **available** inside `Task4`–`Task6`, but all results are discarded after the `isolated` block finishes.

**Use case:** Large speculative computation inside a non-committing scope.

---

### 7. Shared group containing a `parallel-i` and postprocessing

```metta
(shared
  (parallel-i (Task1 $x) (Task2 $y) (Task3 $x $y))
  (Task4 $x)
  (Task5 $y)
  (Task6 $x $y))
```

**Behavior:**

* Outer `shared` allows global binding visibility.
* The `parallel-i` group runs 3 fully isolated tasks concurrently:

  1. `Task1` binds `$x`
  2. `Task2` binds `$y`
  3. `Task3` may use both, but in its own sandbox
* After those tasks complete, the results (if successful) are unified into shared space.

**Postprocessing:**

* `Task4`, `Task5`, and `Task6` run afterward with shared access to `$x` and `$y`.

**Use case:** Isolated inference generation followed by shared knowledge integration.

---

## Unified Summary

MeTTaLog’s multithreading model provides a hybrid of automatic inference and declarative control. It introduces `shared` and `isolated` forms that serve as **smart defaults**, inferring optimal execution strategy based on data dependencies and runtime conditions.

These wrappers allow beginners and intermediate users to write concurrent logic without over-specifying control flow. For example:

* `shared` wraps expressions that reuse or unify variables (e.g. `$x`, `$y`) and may need synchronization
* `isolated` assumes no shared state and permits safe, sandboxed execution

When needed, **explicit annotations** are available:

* Use `sequential` when **execution order matters** (e.g., logging, I/O, `add-atom`, `remove-atom`)
* Use `parallel` when you have **manually verified** independence or commutative effects

This model is designed to:

* Let experts control evaluation precisely
* Let intermediate users write flexible, parallel-safe code
* Let beginners explore without breakage or nondeterministic failures

**Why `sequential` and `parallel` still exist:**

Static inference cannot always detect dangerous global effects, such as:

* Modifications to shared state (`add-atom`, `remove-atom`)
* External I/O (user interfaces, logs)

In these cases, using `sequential` is mandatory to ensure correctness. `parallel` signals a confident assertion that side effects are benign or isolated.

**Best practices:**

* Default to `shared` or `isolated` unless you're sure
* Use `sequential` and `parallel` to enforce semantic correctness or optimize for performance
* Think of `shared` like Lisp's `progn`: a neutral ordered container that defers concurrency decisions

This declarative strategy lets the runtime and programmer **cooperate** in choosing the safest and fastest execution model.


---

## Object Isolation: `snapshot` vs `transaction`

MeTTa supports execution blocks with strict isolation guarantees using `snapshot` and `transaction`. These control whether the system commits changes or rolls them back after a logic block executes.

| Construct     | Commits Changes? | Reentrant? | Rollback on Failure? | Use Case                       |
| ------------- | ---------------- | ---------- | -------------------- | ------------------------------ |
| `snapshot`    | ❌ (never)        | ✅          | ✅                    | Read-only or speculative logic |
| `transaction` | ✅ (on success)   | ❌          | ✅                    | Batched or atomic side effects |

---

### 🔧 Snapshot & Transaction Primitives

These are exposed via the thread system (from `!(import &self threads)`):

| Function                      | Purpose                              |
| ----------------------------- | ------------------------------------ |
| `thread:snapshot!`            | Run block, discard changes           |
| `thread:transaction!`         | Run block, commit changes on success |
| `thread:current-transaction!` | Return current transaction context   |
| `thread:transaction-updates!` | List pending side effects            |
| `thread:show-transaction!`    | Pretty-print pending commit actions  |

---

### 🔍 `snapshot`: Isolated, Read-Only

```metta
(snapshot
  (= (temp-fn $x) (* $x 2))
  (temp-fn 4))
```

> `temp-fn` is not retained globally — it exists only within this block.

**Use Cases:**

* Simulating "what if?" logic
* Optimistic reads
* Avoiding accidental mutations
* Query planning and dry-runs

---

### 🔒 `transaction`: Commit-on-Success

```metta
(transaction
  (= (shared-fn $x) (+ $x 1)))
```

> `shared-fn` is only defined globally if no failures occur in the block.

**Use Cases:**

* Multi-step updates
* Controlled side effects (`add-atom`, `remove-atom`)
* Safe graph mutation or knowledge base editing

---

### 🧠 Safe Multi-Step Update

```metta
(transaction
  (remove-atom (balance Alice $b))
  (add-atom (balance Alice (- $b 100)))
  (print "Debited $100 from Alice"))
```

If any subgoal fails, **no changes are made**. This transactional pattern is useful for atomic state updates.

---

### ✅ Best Practices

* Use `snapshot` when:

  * You need speculative logic
  * You want a read-only version of the world
  * You are evaluating possibilities without consequences

* Use `transaction` when:

  * Multiple changes must be committed atomically
  * You want to guarantee rollback on error
  * You're modifying a shared world model

* Avoid I/O inside `transaction` unless the runtime supports side-effect buffering or compensation.

---

### 🧪 Example (Combined Use)

```metta
!(snapshot
  (= (score-preview $name)
    (sequential
       (add-atom &scores (score $name 42))
       (match &scores (score $name $s) $s))))

!(transaction
  (sequential 
    (remove-atom &self (status Bob waiting))
    (add-atom &self (status Bob active))))
```

This pattern ensures **correct reasoning and mutation separation**, making MeTTa safe for concurrent and logic-dependent workflows.

---




### 🚦 Core Threading

| Function                        | Description                   |
| ------------------------------- | ----------------------------- |
| `thread:spawn!`                 | Create thread with expression |
| `thread:spawn-lazy!`            | Lazy thread eval              |
| `thread:async!`                 | Async spawn, detached         |
| `thread:await!`, `await-token!` | Wait for result               |
| `thread:timeout!`               | Timeout wrapper               |
| `thread:completed?`             | Check if finished             |
| `thread:limit-results!`         | Cap result count from thread  |

---

### 🧠 Execution Groups

| Function             | Description                   |
| -------------------- | ----------------------------- |
| `thread:hyperpose!`  | Run all and collect results   |
| `thread:race!`       | Return first completed result |
| `thread:limit-time!` | Impose deadline               |

---

### 🔒 Synchronization

| Function               | Description         |
| ---------------------- | ------------------- |
| `thread:mutex-create!` | Make a new lock     |
| `thread:mutex-lock!`   | Acquire mutex       |
| `thread:mutex-unlock!` | Release mutex       |
| `thread:mutex-with!`   | Scoped lock context |

---

### 💬 Message Queues

| Function                  | Description             |
| ------------------------- | ----------------------- |
| `thread:queue-create!`    | Create a message queue  |
| `thread:send-message!`    | Send message to a queue |
| `thread:receive-message!` | Receive from queue      |

---

### 🧹 Thread Lifecycle

| Function                     | Description                  |
| ---------------------------- | ---------------------------- |
| `thread:join!`               | Wait for thread completion   |
| `thread:detach!`             | Let thread run independently |
| `thread:suspend!`, `resume!` | Pause and resume             |
| `thread:cancel!`             | Force thread termination     |
| `thread:status!`             | Query thread status          |
| `thread:self!`               | Get current thread ID        |
| `thread:list!`               | List all running threads     |
| `thread:sleep!`              | Sleep for seconds            |
| `thread:set-priority!`       | Adjust thread priority       |

---

### ❌ Error Handling

```metta
(thread:error! $thread "Failed to join")
```

---

## 🧠 Threads vs Engines

### Threads

```metta
(thread:spawn! (my-computation))
```

* Executes expression in its own engine
* Tied to thread lifecycle

### Engines

```metta
(engine:create $x (between! 1 10) $e ())
(engine:next $e $val)
```

* Can be stepped
* Shared across threads

| Use Case                | Thread API | Engine API |
| ----------------------- | ---------- | ---------- |
| Fire-and-forget tasks   | ✅          | ❌          |
| Lazy streams            | ❌          | ✅          |
| Share across threads    | ⚠ risky    | ✅          |
| Debuggable/step control | ❌          | ✅          |

---

## 📌 What is a Template?

A **template** is a projection over engine results:

```metta
(bind! &eng (engine:create $x (member $x (1 2 3))))
(engine:next &eng) ; → 1, 2, 3
```

