

Despite the significant interest in type theory and the Curry-Howard correspondence within the functional programming community, as highlighted by P. Wadler, it is evident that only a minimal subset of logic programs—those that are relatively straightforward and less complex—can be effectively translated into functional programming. This observation does not undermine the importance of Curry-Howard or type theory. Rather, it emphasizes that these frameworks have not been and are unlikely to become practical methods for implementing logic programming. In light of this, our system has been designed to transform functions into a canonical form of relations by adding an extra argument to handle return values:

For instance, a function body like:

```lisp
(= (my-function $b) (foo a b (bar c d)))
```
converts to...
```clif 
(==> 
  (and (bar c d $barRet1) 
        (foo a b $barRet1 $fooRet2))
  (my-function $b $fooRet2))
```
----------------------------

```lisp
(= (fib $n)
  (if (== $n 0) 0
  (if (== $n 1) 1
    (+ (fib (- $n 1)) (fib (- $n 2))))))

!(println (fib 44444))

```
converts to...
```clif

(==>
  (xor (and (== $n 0) (== $fibFinal 0))
  (xor (and (== $n 1) (== $fibFinal 1))
        (and (> $n 1)
          (minus $n 1 $nMinus1)
          (minus $n 2 $nMinus2)
          (fib $nMinus1 $fibRetNMinus1)
          (fib $nMinus2 $fibRetNMinus2)
          (plus $fibRetNMinus1 $fibRetNMinus2 $fibFinal))))

    (fib $n $fibFinal))

(==> 
  (and (fib 44444 $fibFinal) 
       (println! $fibFinal $QR)
  (query $QR))
```

----------------------------


```lisp
!(match &self (, (chr $g "chr16") (start $g $start) (end $g $end))
    (if (and (> $start 500) (< $end 2000)) $g (empty)))
```
converts to...
```clif
(==>
    (and (and (chr $g "chr16") (start $g $start) (end $g $end)
    (> $start 500) (< $end 2000)))

  (query $g))  ;; $g was the result returned from match
```
----------------------------

Using our approach, we are to combine all of MeTTa's functional programming (`eval`-ish) and logic programming (`match`-atomspacey) into one model to optimize.

 Since both are in the relational model we can alter the execution order to create algebraic (multidirectional processing).  Segments of reordered code is JIT-compiled on WAM and further optimized using conventional computational techniques.

This transformation inside our Atomspace allows  automated theorem provers and inference engines _(such as those created for SUMO, CycL, FOL, and HOL)_ all work together be able to run programs as if they are knowledge-base queries. 

**The bigger deal is we have made it possible for Probablistic reasoning inference able to run programs.**

 
We are to combine MeTTa's functional programming (`eval`-ish) and logic programming (`match`-atomspacey programming) into one "relational programming" model.

Segments of code in this canonical form is able to be reordered in several possible ways because 

 - **Steadfast relations**: All input and outputs to functions become hybrid mixures of both. (Functions can have multiple outputs as well as multiple inputs (or no inputs and/or no outputs)) Our system vets the possible interdependancies so it can safely change the execution flow of programs (example the classic `append` function no longer only acts like one to three different functions, in our system it acts like it was written to become 81 different function implementations).

 - **One semi-deterministic pass**: One semi-deterministic pass over non- determistic code creates a "what is needed to bind this variable?" in which our planner in one determinisitc swipe has the smallest possible version of nondetermism as well as `Algebraic CLP`

Before durring and after segments are JIT-compiled on WAM and further optimized using conventional computational techniques.

The big deal is the transformation allows  automated theorem provers and inference engines _(such as those created for SUMO, CycL, FOL, and HOL)_ to be able to 

- **Execute programs as if they are knowledge-base queries**: Using the dozens query optimization methods that are otherwise impossible in the realm of program execution


Refer to the [./docs/OVERVIEW.md](docs/OVERVIEW.md) in this repository.

- The biggest novel gain of this implimention is we have made it possible for Probablistic reasoning inference (or even Neural Nets) to run symbolic code.


### Ontological and Type System Integration
Our system allows for the integration of ontological types (e.g., from SUMO and RDF) with regular and dependent types on free variables, values, objects and functions. As a result, developers can ontological types such as `StringHoldingALikeableStatement` or `StringHoldingWhoKnowsWhat` or even if we dont know the datatype we can still use `ALikeableStatement` or `WhoKnowsWhat`.  This enhancement allows complex semantic relationships, datatypes and traditional type theories to work off of each other. As types are no longer _just part of your program_ they can be used for  deductions and inductions both inside and outside of your program. For example your program can operate using epistemic and deontontic reasoning at runtime about what it is doing.


### Extensive Language Support
Our compilation technique has been effectively applied to: Python*, Common Lisp*, Curry* and HVM* by converting their individual programs' source code (in AST form) into our intermediate representation. In the Python experiment it allowed python code for a novel solver for the visual Abstract Reasoning corpus that won the 2022 Lab42 challenge. Michael Hodel purposely overly specialized it in such a way that if you changed something like the color inside of a riddle in the test set it would break his solver's ability to solve it (Much liek a "single pixel attack").  Our tranpilation framework didnt mind and was able to take the code which was purposly written overly specialized for 800 riddles and find new pathways in his code.  Afterwhich we were able to use his code as a slightly general purpose solver. Exciting as that is, more exciting is that MeTTa can update and change the semantics of his code while running. Our success with the previous languages listed indicate some amazing things it would be able to do with Haskell, Verse,  Java, Idris, Agda, Rust, Oz and OcamML etc etc.

_note: Our model works for event base programming like GUI systems and programs with side effects._

For more details  refer to the [docs/OVERVIEW.md] in this repository.


# Conversion of Programs to Predicates in Inference Engines

This documentation outlines the methodology for transforming traditional function-based programming constructs into predicates suitable for logic programming and inference engines. This process leverages the hybrid nature of parameters, akin to those observed in logic programming, to provide a more adaptable and potent approach to representing computational logic.

## Overview

Converting functions to predicates involves reinterpreting the traditional execution model to fit within a predicate logic framework. This conversion allows inference engines to process and reason about the computations more effectively.

## Conversion Process Using S-expressions

### Step 1: Function Representation in S-expressions

- **Original Function Representation:**  
 - In a conventional programming context, an addition function might be represented as `= (add $X $Y) $Z`, which denotes that `$Z` is the result of adding `$X` and `$Y`.

### Step 2: Conversion to Predicate Form

- **Converted Predicate Representation:**  
 - The transformation involves reformatting the function into a predicate by including the output as an additional argument, resulting in `(add $X $Y $Z)`. In this format, `$Z` is integrated into the predicate, establishing a tripartite relationship among `$X`, `$Y`, and `$Z`.

### Step 3: Handling Nested Functions

- **Original Nested Function:**  
 - An example of a nested function could be `(let $B (multiply (add $X $Y) $A) ...)`, implying that `$B` is the outcome of multiplying the sum of `$X` and `$Y` by `$A`.

- **Conversion to Predicates:**  
 - Nested functions are decomposed into separate predicates: `(add $X $Y $Sum)` for addition, and `(multiply $Sum $A $B)` for multiplication. Here, `$Sum` serves as a bridge between the two operations, acting as both an output from the addition and an input to the multiplication.

## Example with Hybrid Parameters

Parameters in logic programming can dynamically serve as inputs or outputs, based on the context of their usage. This flexibility is exemplified in the conversion process.

### Addition Example

- **Predicate:** `(add $X $Y $Z)`  
 - This predicate validates the addition operation, illustrating the dynamic roles that parameters can play within a logic programming framework.

### Nested Operation Example

- **Predicates:** `(add $X $Y $Sum)` and `(multiply $Sum $A $B)`  
 - These predicates represent a nested operation, demonstrating how `$Sum` transitions from being the result of addition to being a factor in multiplication.

## Advantages of This Approach

- **Flexibility:** This method supports both forward and backward reasoning, enabling complex problem-solving capabilities.  
- **Expressiveness:** The declarative nature of logic programming facilitates a more intuitive description of computational problems.  
- **Efficiency:** By leveraging pattern matching and logical inference, this approach can lead to more effective problem-solving strategies.

Through this conversion methodology, we harness the full potential of logic programming, opening new avenues for computational reasoning and problem-solving within inference engines.


### No loops or explosions
In stark contrast to the von Neumann architectures' sequential execution pattern, our system embodies a distinct and specialized computational model that thrives within the realm of functional and logic programming. Leveraging the Warren Abstract Machine (WAM), our approach utilizes registers, a tailored instruction set, a stack-based execution model, and a unique approach to memory management. These features collectively enable superior execution of logic programs, where logical constraints and backtracking are crucial.
