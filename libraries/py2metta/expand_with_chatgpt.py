import ast
import sys
from openai import OpenAI

client = OpenAI()

def extract_functions(path):
    with open(path) as f:
        tree = ast.parse(f.read())
    return [node for node in tree.body if isinstance(node, ast.FunctionDef)]

def format_prompt(func):
    src = ast.unparse(func)
    return f"""You are a Python readability assistant.

Expand the following Python function into a clean, readable version:
- Use meaningful variable names
- Break apart chained function calls and inlined expressions
- Make all intermediate steps explicit and easy to follow
- Preserve the logic exactly

Output only the rewritten Python function — no extra text or explanation.

```python
{src}
```
"""

def ask_chatgpt(prompt):
    response = client.chat.completions.create(
        model="gpt-4o",
        messages=[{"role": "user", "content": prompt}],
        temperature=0.3
    )
    return response.choices[0].message.content.strip()

def main():

    import datetime
    
    input_file, output_file = sys.argv[1], sys.argv[2]
    functions = extract_functions(input_file)
    
    # Write header with timestamp
    with open(output_file, 'w') as f:
        f.write(f"## Expanded by ChatGPT on {datetime.datetime.now().isoformat()}\n\n")
    
    # Process each function and append individually
    for func in functions:
        prompt = format_prompt(func)
        expanded_function = ask_chatgpt(prompt)
    
        with open(output_file, 'a') as f:
            f.write(expanded_function + "\n\n")


if __name__ == "__main__":
    main()
