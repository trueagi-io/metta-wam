# MeTTa Language — Prolog Implementation Files

This directory contains the main Prolog and Python components of the MeTTa-WAM system — an implementation of the MeTTa language using Prolog.

## 📂 Directory Structure

| File / Module | Description |
|---------------|-------------|
| [`corelib.metta`](corelib.metta) | Core standard library definitions in MeTTa syntax. |
| [`init.default.metta`](init.default.metta) | Default initial MeTTa context; loaded at REPL or runtime. |
| [`repl.default.metta`](repl.default.metta) | REPL-specific startup MeTTa code. |
| [`stdlib_mettalog.metta`](stdlib_mettalog.metta) | Standard library atoms and logic for MeTTaLog. |

---

## 🧠 Compiler Files

| File | Description |
|------|-------------|
| [`metta_compiler.pl`](metta_compiler.pl) | Main compiler frontend. Coordinates parsing, conversion, and code gen. |
  [`metta_compiler_lib.pl`](metta_compiler_lib.pl) | Shared predicates and helpers used by the main compiler. |
|
  [`metta_compiler_douglas.pl`](metta_compiler_douglas.pl) | Experimental or customized compiler logic by Douglas. |
| [`metta_compiler_lib_douglas.pl`](metta_compiler_lib_douglas.pl) | Douglas’s library extensions for the compiler. |
 
  [`metta_compiler_roy.pl`](metta_compiler_roy.pl) | Alternative or experimental compiler by Roy. |
| [`metta_compiler_lib_roy.pl`](metta_compiler_lib_roy.pl) | Roy’s library extensions for the compiler. |

---

### 🧩 Core System Logic

| File | Description |
|------|-------------|
| [`metta_corelib.pl`](metta_corelib.pl) | Core predicates and behaviors of the MeTTa language. |
| [`metta_eval.pl`](metta_eval.pl) | Interpreter and evaluation rules. |
| [`metta_interp.pl`](metta_interp.pl) | Intermediate interpreter logic. |
| [`metta_runtime.pl`](metta_runtime.pl) | Execution environment and runtime services. |
| [`metta_loader.pl`](metta_loader.pl) | Loads `.metta` files and prepares them for execution. |
| [`metta_space.pl`](metta_space.pl) | Logical space and storage for atoms. |
| [`metta_subst.pl`](metta_subst.pl) | Substitution logic used during evaluation. |
| [`metta_types.pl`](metta_types.pl) | Type system and inference logic. |
| [`metta_typed_functions.pl`](metta_typed_functions.pl) | Typed function support and registration. |

---

### 🧪 Debugging & Testing

| File | Description |
|------|-------------|
| [`metta_debug.pl`](metta_debug.pl) | Debugging utilities and trace helpers. |
| [`metta_testing.pl`](metta_testing.pl) | Unit and integration tests for the MeTTa system. |
| [`metta_utils.pl`](metta_utils.pl) | Miscellaneous utilities (term manipulation, helpers, etc). |

---

### 🧬 PFC (Interpreter Agent Chaining)

| File | Description |
|------|-------------|
| [`metta_pfc_base.pl`](metta_pfc_base.pl) | Base rules for forward chaining for Agent. |
| [`metta_pfc_support.pl`](metta_pfc_support.pl) | Agent integration and helpers. |
| [`metta_pfc_debug.pl`](metta_pfc_debug.pl) | Debugging tools for Agent reasoning. |

---

### 🧾 REPL and Server

| File | Description |
|------|-------------|
| [`metta_repl.pl`](metta_repl.pl) | Interactive REPL interface. |
| [`metta_server.pl`](metta_server.pl) | HTTP/IPC server endpoint for remote execution. |
| [`metta_threads.pl`](metta_threads.pl) | Thread handling for concurrent evaluation. |

---

### 🧠 Proofs and Reasoning

| File | Description |
|------|-------------|
| [`metta_proof.pl`](metta_proof.pl) | Proof tracking and explanation system. |
| [`metta_ontology.pfc.pl`](metta_ontology.pfc.pl) | Ontology-related facts/rules for reasoning. |
| [`metta_mizer.pl`](metta_mizer.pl) | Optimization and minimization logic. |
| [`metta_improve.pl`](metta_improve.pl) | Proof refinement or simplification routines. |

---

### 🔗 Python Interop

| File | Description |
|------|-------------|
| [`metta_python.pl`](metta_python.pl) | Glue layer to interface with embedded Python. |
| [`metta_python_builtin.py`](metta_python_builtin.py) | Custom Python functions exposed to MeTTa. |
| [`metta_python_override.py`](metta_python_override.py) | Overrides or intercepts for MeTTa/Python behavior. |
| [`metta_python_proxy.py`](metta_python_proxy.py) | Proxy system between Prolog and Python values. |
| [`metta_python_patcher.py`](metta_python_patcher.py) | Patch system for Python side of MeTTa runtime. |
| [`metta_python_hyperon.py`](metta_python_hyperon.py) | Integration with Hyperon-specific Python logic. |

---

## 🧪 Dev Host File

| File | Description |
|------|-------------|
| `Sav.*.MeTTaLog` | Device Specific Executable for Windows, MacOSX or Linux. |

---

## 🔗 Documentation

If available, documentation and contribution guidelines can be found at:

- [MeTTa-WAM GitHub Repository](https://github.com/trueagi-io/metta-wam)


