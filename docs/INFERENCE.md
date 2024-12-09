

# Mettalog: Extended Inference and Code Execution Engine

Mettalog is an advanced inference engine inspired by EProver. In addition to logical reasoning, we've extended Mettalog to emulate running code, allowing for a powerful combination of logical inference and procedural execution.

## Features

- **Logical Inference:** Solve complex logical problems and theorems.
- **Code Emulation:** Execute or simulate programming code seamlessly integrated with logical inference.
- **Knowledge Representation:**
  - **MeTTa KR:** Custom Knowledge Representation for logical rules and axioms.
  - **SUMO:** Suggested Upper Merged Ontology for sophisticated ontological representation.

## Axioms and Logical Rules

Here's an example of an axiom in both MeTTa KR and SUMO KR:

### MeTTa KR

In Mettalog's native format:

```lisp
(= (next-number $x) (+ 1 $x))
```

## Examples

### Logical Inference

Given a set of axioms and rules, Mettalog can infer new facts. For example:

#### MeTTa KR

Axioms:
```lisp
(= (next-number $x) (+ 1 $x))
(= (even $x) (if (= (mod $x 2) 0) true false))
```

Query:
```lisp
(next-number 5)
```

Mettalog's inference:
```lisp
6
```

#### SUMO KR

Axioms:
```lisp
(equals (next-number ?x) (AdditionFn ?x 1))
(equals (even ?x) (case (equal (RemainderFn ?x 2) 0) true false))
```

Query:
```lisp
(next-number 5)
```

Mettalog's inference:
```lisp
6
```

### Code Emulation

Mettalog can emulate running code to solve problems that require procedural steps. Here’s an example in Python:

```python
def next_number(x):
    return x + 1

def is_even(x):
    return x % 2 == 0

next_number(5)  # Output: 6
is_even(5)  # Output: False
```

Emulated in Mettalog:

```lisp
(defun next-number (x) (+ x 1))
(defun is-even (x) (== (mod x 2) 0))

(next-number 5)  ; Output: 6
(is-even 5)      ; Output: false
```

### Combined Inference and Emulation

Mettalog can combine logical inference and code emulation for more complex scenarios. For example, finding the next even number after a given number:

#### Logical Inference

Axioms:
```lisp
(= (next-number $x) (+ 1 $x))
(= (even $x) (if (= (mod $x 2) 0) true false))
```

Procedure:
```lisp
(defun next-even-number (x)
  (if (even (next-number x))
    (next-number x)
    (next-number (next-number x))))
```

Query:
```lisp
(next-even-number 5)
```

Mettalog's result:
```lisp
6
```

#### Code Emulation

Python code:
```python
def next_number(x):
    return x + 1

def is_even(x):
    return x % 2 == 0

def next_even_number(x):
    next_num = next_number(x)
    if is_even(next_num):
        return next_num
    else:
        return next_number(next_num)

next_even_number(5)  # Output: 6
```

Emulated in Mettalog:
```lisp
(defun next-number (x) (+ x 1))
(defun is-even (x) (== (mod x 2) 0))

(defun next-even-number (x)
  (if (is-even (next-number x))
    (next-number x)
    (next-number (next-number x))))

(next-even-number 5)  ; Output: 6
```

## Conclusion

Mettalog bridges the gap between logical reasoning and code execution, providing a powerful tool for complex problem-solving. Whether you're working with logical theorems or procedural code, Mettalog offers a seamless integration of both worlds.

## Installation

To install Mettalog, follow these steps:
1. Clone the repository: `git clone https://github.com/trueagi-io/metta-wam.git`
2. Navigate to the project directory: `cd mettalog`
3. Install dependencies: `pip install -r requirements.txt`
4. Run the tests to ensure everything is set up correctly: `pytest`

## Usage

To use Mettalog, import the main module and start defining your axioms and rules:

```python
from mettalog import Engine

engine = Engine()

# Define your axioms and rules here
engine.add_axiom('(= (next-number $x) (+ 1 $x))')
engine.add_axiom('(= (even $x) (if (= (mod $x 2) 0) true false))')

# Query the engine
result = engine.query('(next-number 5)')
print(result)  # Output: 6
```

For more examples and detailed documentation, visit our [wiki](https://github.com/yourusername/mettalog/wiki).

## License

Mettalog is licensed under the MIT License. See the [LICENSE](LICENSE) file for more information.

## Contributing

We welcome contributions from the community! Please read our [contributing guide](CONTRIBUTING.md) to get started.

```

This README provides a comprehensive overview of Mettalog, including its features, examples of logical inference and code emulation, installation instructions, usage, and contribution guidelines. Let me know if you need any more details or further adjustments!
