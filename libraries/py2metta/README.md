# 🧠 py2metta Logic Translation Pipeline

This project provides a full transformation pipeline for symbolic reasoning research and logic-based AI. Starting from compact Python code, it expands, reformats, and translates it into both **Prolog** and **MeTTa** using multiple strategies.

---

## 🧩 Purpose

To make opaque or compact Python logic functions easier to reason about by converting them into declarative formats:
- ✅ Uncompressed Python (for readability)
- ✅ Prolog (logic programming)
- ✅ MeTTa (symbolic reasoning language)

---

## 🔐 API Key Setup

You must set your OpenAI API key before using any GPT-based tools:

```bash
export OPENAI_API_KEY=your-api-key-here
```

Place this in your `.bashrc`, `.zshrc`, or shell profile for convenience.

---

## 🔄 Pipeline Overview

You can run the full pipeline using:

```bash
./py2metta.sh arc_math_utils.py
```

This will generate:

| Stage                        | Output Files                              |
|-----------------------------|-------------------------------------------|
| ChatGPT Expansion           | `arc_math_utils_gpt_1.py`, `gpt_2.py`     |
| AST Expansion               | `arc_math_utils_ast_1.py`, `ast_2.py`     |
| Python → Prolog             | `*.pl`                                     |
| Python → MeTTa              | `*.metta`                                  |
| Prolog → MeTTa              | `*.prolog.metta`                           |

---

## 🧠 Tool Descriptions

### 1. `expand_with_chatgpt.py`
Uses OpenAI GPT-4 to rewrite compact Python logic into:
- Step-by-step logic
- Descriptive variable names
- No functional chaining

### 2. `expand_with_ast.py`
Does the same using Python's AST:
- Flattens expressions
- Splits ternaries into `if/else`
- Replaces inline ops with temporary variables

### 3. `converter.py`
Translates Python logic into **MeTTa**, with:
- Output variable placed last
- S-expression format using `(= ...)`
- `do-match &self` blocks for scoping

### 4. `python_to_prolog.py`
Uses ChatGPT to translate Python into Prolog clauses:
- Each clause is output-last
- Type guards (`integer/1`, `is_tuple/1`, etc.) included
- Original Python shown as a `/* ... */` comment block

### 5. `prolog_to_metta.py`
Converts Prolog clauses into MeTTa relations using GPT:
- Clause → `do-match &self`
- Preserves structure and unification logic

---

## 📁 Output Patterns

Each transformation stage produces filenames like:

- `arc_math_utils_gpt_1.py`
- `arc_math_utils_gpt_1.pl`
- `arc_math_utils_gpt_1.metta`
- `arc_math_utils_gpt_1.prolog.metta`

This makes it easy to trace each output back to its origin.

---

## ✅ Run Everything

Just do:

```bash
chmod +x py2metta.sh
./py2metta.sh arc_math_utils.py
```

---

Let us know if you'd like to add:
- 🔁 Loop unrolling
- 🧪 Test generation
- 🧠 Semantic annotations
