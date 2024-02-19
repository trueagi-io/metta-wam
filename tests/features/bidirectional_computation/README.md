# Bidirectional Computation in Functional Programming

This document explains the unique feature of bidirectional (or reversible) computation in our functional programming language that uses a non-determinsitic backend with CLP(FD).  The non-determinsitic backend is what allows MeTTaLog programs to run backwards in time inducing the inputs that would have created some outputs.

## Feature Overview

Bidirectional computation allows expressions to be evaluated in both forward and reverse directions. This capability is enabled by the integration of Constraint Logic Programming over Finite Domains (CLP(FD)) with a non-determinsitic backend.

## Advantages

- **Flexibility**: Supports versatile and powerful function definitions.
- **Efficiency**: Reduces the need for separate logic for reverse computations.
- **Expressiveness**: Enhances the ability to express a wide range of computational problems.

## Example

- Forward: `X = factorial(5)` results in `X` being 120.
- Reverse: `120 = factorial(X)` finds `X` such that `X` is 5.



### Forward Mode

In this traditional mode, functions compute outputs from given inputs. For example:

```prolog
X = factorial(5).
```

This expression computes the factorial of 5, resulting in `X` becoming 120.

### Reverse Mode

This unique mode computes possible inputs that produce a given output. For example:

```prolog
120 = factorial(X).
```

Here, the system determines the value of `X` such that the factorial of `X` equals 120.

## Mechanism

The MeTTaLog backend with CLP(FD) enables reversible computation. CLP(FD) allows for defining relations among variables with constraints, and the MeTTaLog engine utilizes these constraints for bidirectional inference.

## Applications

Bidirectional computation is beneficial in various fields, including:

- Diagnostics
- Planning
- Problem-solving

It is particularly useful where reverse inference is as crucial as forward computation.

