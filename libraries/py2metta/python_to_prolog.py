import ast
import sys
from openai import OpenAI

client = OpenAI()

def extract_functions(path):
    with open(path) as f:
        tree = ast.parse(f.read())
    return [node for node in tree.body if isinstance(node, ast.FunctionDef)]

def python_to_prolog_comment(src):
    lines = src.strip().split('\n')
    return '\n'.join(f"% {line}" for line in lines)

def format_prompt(func):
    src = ast.unparse(func)
    doc = ast.get_docstring(func) or "No documentation provided"
    func_name = func.name
    return f"""You are a logic programming expert.

Convert the following Python function into idiomatic Prolog.

✅ Output Format

- One clause per control path
- Use predicates like `if`, `for`, `tuple_index`, etc.
- Represent values as facts, unifications, or expressions
- Use underscores (_) for ignored variables
- Use descriptive variable names (e.g. X, Y, Output)
- Do not use Python syntax or comments

✅ Examples

Python:
def add(a, b):
    return a + b

Prolog:
add(A, B, Out) :-
    Out is A + B.

Python:
def max2(x, y):
    if x > y:
        return x
    return y

Prolog:
max2(X, Y, X) :- X > Y.
max2(X, Y, Y) :- X =< Y.

Python:
def filter_even(xs):
    return [x for x in xs if x % 2 == 0]

Prolog:
filter_even(Xs, Out) :-
    findall(X, (member(X, Xs), 0 is X mod 2), Out).

Python:
{src}
"""


def ask_chatgpt(prompt):
    response = client.chat.completions.create(
        model="gpt-4o",
        messages=[{"role": "user", "content": prompt}],
        temperature=0.2
    )
    return response.choices[0].message.content.strip()

def convert_python_to_prolog(func):
    prompt = format_prompt(func)
    try:
        return ask_chatgpt(prompt)
    except Exception as e:
        print(f"❌ Error converting {func.name}: {e}")
        return None

def main():
    if len(sys.argv) != 3:
        print("Usage: python python_to_prolog.py <input.py> <output.pl>")
        sys.exit(1)

    input_file, output_file = sys.argv[1], sys.argv[2]
    functions = extract_functions(input_file)

    with open(output_file, 'w') as f:
        f.write(f"% Prolog code generated from {input_file}\n\n")

    for func in functions:
        python_src = ast.unparse(func)
        comment = python_to_prolog_comment(python_src)

        prolog_code = convert_python_to_prolog(func)
        if not prolog_code:
            print(f"❌ Skipping {func.name}")
            continue

        block = f"{comment}\n{prolog_code}\n"
        print("🔹 Generated Prolog block:\n")
        print(block)

        with open(output_file, 'a') as f:
            f.write(block + "\n")

if __name__ == "__main__":
    main()
