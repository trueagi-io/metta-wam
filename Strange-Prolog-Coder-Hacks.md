Prolog programmers often unconventional techniques to handle scenarios like undefined predicates, conditional execution, or prototyping code. This page explores some of those tricks.

## Hack 1: The `nop/1` Predicate

### Example:

```prolog
nop(_).

do_stuff:- 
  nop(step1),
  step2.
```

### Explanation:

The `nop/1` predicate always succeeds without doing anything, regardless of the argument passed to it. This trick is useful for temporarily disabling parts of code while maintaining readability and structure.

### Why Use It?

- **Placeholders for Disabled or Future Code**: Use `nop/1` as a quick way to "comment out" code without removing it.
- **Debugging and Prototyping**: Selectively deactivate steps in a process without breaking the flow of the program.
- **Readable and Modular Code**: Even when a step is inactive, it remains visible in the code, preserving the overall structure.

---

## Hack 2: Delayed Code Definition with `call(call, ...)`

### Example:

```prolog
do_something:- 
  call(call, some_pred(stuff)).
```

### Explanation:

In SWI-Prolog, when you use `call(some_pred(stuff))` and `some_pred/1` is not yet defined, you’ll receive a warning like "Procedure some_pred/1 does not exist." However, by wrapping the call in an extra `call`, like `call(call, some_pred(stuff))`, you can reference a predicate that hasn’t been defined yet without triggering this warning.

### Why Use It?

- **Deferring Definition**: This trick is handy when you want to reference a predicate that you’ll define later. It allows you to build the structure of your code without needing every part implemented upfront.
- **Modular Development**: You can design complex systems where certain predicates are loaded or defined dynamically, while still referencing them early in the code.
- **Avoiding Compilation Warnings**: SWI-Prolog is smart enough to issue warnings about undefined predicates when you use `call(some_pred(stuff))`. Using `call(call, some_pred(stuff))` suppresses these warnings until the predicate is defined.

### How It Works:

- The first `call` effectively delays the evaluation of the inner `call(some_pred(stuff))`, allowing Prolog to treat it as a more dynamic or indirect reference.
- Once `some_pred/1` is eventually defined, the code will run as expected.

### When to Use It:

- For scenarios where predicates are loaded dynamically, such as in modular or plugin-based systems.
- When you’re building code that evolves over time, and you want to define certain predicates later, use `nop/1` instead.
