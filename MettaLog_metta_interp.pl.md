# Overview of eval_## Modes in MeTTaLog

In MeTTaLog, different evaluation modes (eval_20, eval_40, and eval_70) provide varying degrees of control over how arguments are processed and how failures are handled during function evaluation. Below, we detail the distinctions between these modes and their appropriate use cases.

## eval_20: Manual Argument Evaluation

### Behavior
- When using `eval_20`, the function is responsible for explicitly evaluating its arguments using `eval_args`. This is essential when argument evaluation needs to be conditional, delayed, or selectively controlled.
- This mode is commonly applied in "special forms," where premature evaluation would be incorrect or lead to unwanted behavior.

### Example Use Cases
- **Conditional Expressions:** In an expression like `(if $cond $then $else)`, evaluating all arguments upfront is incorrect because either `$then` or `$else` should be evaluated based on the outcome of `$cond`.
- **Short-Circuiting Logic:** In expressions like `(or $code1 $code2)`, evaluating `$code2` is unnecessary if `$code1` already returns a truthy value.

### Failure Handling
- In `eval_20`, if evaluation fails, the function can prevent a value from being returned, giving the user full control over error management.

## eval_40: Manual Evaluation with Guaranteed Return

### Behavior
- Similar to `eval_20`, the function must manually evaluate arguments using `eval_args` and can determine how each argument is processed.
- However, unlike `eval_20`, this mode requires that a value is returned even if an error occurs during evaluation.

### Example Use Cases
- Scenarios where a fallback value is needed despite evaluation errors, ensuring that execution always results in a defined output.

## eval_70: Pre-Evaluated Arguments (Except MeTTa "Atoms")

### Behavior
- In `eval_70`, most arguments are pre-evaluated before being passed to the function. This simplifies function design by removing the need to manually evaluate each argument.
- **Important Note:** Symbols and constants are not evaluated automatically and are passed as-is, making them available for symbolic manipulation or lazy evaluation within the function.

### Example Use Cases
- Standard functions that do not require special control over argument evaluation, allowing for straightforward functional programming where arguments are predictably pre-evaluated.

## Summary
- **eval_20:** Provides the most control, requiring manual evaluation and allowing full error handling, ideal for special forms.
- **eval_40:** Similar control as `eval_20`, but ensures a return value even if evaluation fails.
- **eval_70:** Most arguments are pre-evaluated, except when declared not to, making it suitable for regular functions that don’t need custom evaluation logic.

---

# Context and &self in MeTTa Evaluation

## eval Starts from the &self Atomspace
- The `eval` function always begins its evaluation with the current atomspace, typically referred to as `&self`.
- This atomspace (`&self`) serves as the default context for the evaluation process, containing all the symbols, functions, and rules available during evaluation.
- When `eval` is called, it uses this context unless explicitly overridden, ensuring access to the definitions and state within the current module or environment.

## eval_args and Other eval_## Functions Expect Context to Be Passed
- Functions like `eval_args` and other specialized `eval_##` functions are not directly tied to `&self`. Instead, they expect the evaluation context (like an atomspace) to be passed explicitly.
- This design allows flexibility, enabling these functions to be used in various contexts, not just within the default `&self` space.

### Why This Separation?
- The main `eval` function acts as a "controller" that sets up the environment and manages top-level evaluation.
- The context-sensitive nature of `eval_args` and other `eval_##` functions allows them to be adaptable and reusable across different scopes without being bound to the default atomspace.

### Overriding the Context
- While `eval` defaults to `&self`, it’s common to override this context when dealing with nested evaluations or alternative atomspaces.
- This override is done by passing the new context to `eval_args` or other specialized evaluation routines.

## Summary of the Process
- **eval:** Always starts with `&self` as the context, ensuring that the current module or environment is the basis for evaluation.
- **eval_args and other eval_## functions:** Expect the context to be passed to them, allowing for flexibility in nested or alternative evaluation contexts.

---

# The Role of Catch/Throw in eval

## Top-Level Control with eval and Catch/Throw Setup
- The `eval` function not only starts from the `&self` atomspace but also sets up control structures for handling errors, backtracking, and non-deterministic choices using a catch/throw mechanism.
- At the start of `eval`, a "catch block" is established to capture exceptions or failures, allowing evaluation to either recover from errors or backtrack gracefully.
- If an error or failure is encountered during evaluation, a "throw" operation can be invoked to exit the current evaluation and transfer control back to the catch block.

### Use Cases
- **Unification Failures:** When a pattern doesn’t match, a throw allows evaluation to attempt alternative branches.
- **Error Handling:** When an unexpected error occurs, a throw can redirect control to a recovery routine or terminate the current path.

## eval_args and Specialized eval_## Functions Participate Within the Established Catch/Throw Context
- Unlike `eval`, which sets up the catch/throw blocks, `eval_args` and specialized `eval_##` functions assume that the control structures are already in place.
- These inner evaluation routines don’t establish their own catch/throw blocks and rely on the top-level `eval` to manage control flow, ensuring consistent handling of errors and branching logic.

### Flow of Control with Catch/Throw
- When `eval` sets up a catch block, it creates a checkpoint. If a failure or error occurs during evaluations, the throw redirects control back to this checkpoint.
- **Example:** If a function application fails due to an invalid argument, `eval_args` might throw an error, passing control back to `eval`, which decides the next step (retry, skip, report, etc.).

### Special Handling of Catch/Throw in eval
- Since `eval` is aware of broader control flow, it decides whether to retry an operation, select a different path, or terminate the evaluation. This decision-making is guided by catch/throw blocks.
- `eval_args` and other specialized functions participate within this context but do not engage in the decision-making.

## Summary of the Catch/Throw Relationship
- **eval:** Sets up catch/throw blocks, establishing a control context that manages errors, backtracking, and non-deterministic branching.
- **eval_args and Specialized eval_## Functions:** Operate within the catch/throw context set by `eval`, evaluating expressions and passing control back in case of errors or failures.
- **Catch/Throw Mechanism:** Provides a robust framework for handling unification failures, errors, and alternative branching, centralizing control within `eval` while allowing flexible nested evaluations.

---

# String Functions and eval_70

String functions implemented by @royward would ideally fit into `eval_70` because they involve straightforward pre-evaluated argument handling. However, there’s an important technical limitation: explicit type information is required for a function to operate within `eval_70`. 

### Why String Functions Should Be in eval_70
- **eval_70** is ideal for functions like string operations that don’t require special argument handling. Instead, they work well with pre-evaluated arguments.

### The Challenge of Missing Type Declarations
- Until recently, around 80% of the MeTTa library lacked proper type declarations for its internals. Without explicit type information, the system cannot reliably classify these functions into `eval_70`.
- As more type annotations are added, it will become easier to move suitable functions into `eval_70`, improving performance and evaluation consistency.

### Current Status
- The situation has improved, and now about 20% of the library remains without type declarations. Continued progress on type annotations will allow more functions, like Roy’s string operations, to be classified into `eval_70`.

---

# Lazy Control in eval_20 for Non-Deterministic Operations

Operations like `intersection`, `unique`, and `union` require fine-grained, lazy control over argument evaluation, making them unsuitable for `eval_70`.

## Detailed Example: Intersection

### Process Overview
1. **First Argument Iteration:**
   - The first argument (e.g., a list or iterator) is iterated element by element.
   
2. **Selective Iteration of Subsequent Arguments:**
   - For each element from the first argument, the second argument is iterated to find a unifiable match.
   - If a match is found early, it pauses, and control returns to the first argument’s iterator.

3. **Handling Exhaustive Iteration:**
   - If the second argument’s iterator completes without a match, results are memoized, and control returns to the first argument.

### Why Lazy Control is Needed
- **Pausing and Resuming Iterators:** Each argument must pause and resume based on whether a match is found.
- **Non-Deterministic Evaluation:** `eval_70` cannot express this control pattern since it demands conditional, step-by-step evaluation.

## Implications
- Operations like `intersection`, `union`, and `unique` must remain in `eval_20`, where:
  - Arguments are evaluated lazily.
  - The order and timing of evaluation are fully controlled based on intermediate results, supporting non-deterministic logic.

---

This wiki page provides a detailed overview of key concepts in MeTT

aLog's evaluation process, guiding developers through the rationale behind different evaluation modes and their applications.
