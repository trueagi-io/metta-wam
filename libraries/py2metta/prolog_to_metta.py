import sys
import re
from openai import OpenAI

client = OpenAI()

def format_metta_prompt(prolog_clause):
    return f"""You are a symbolic programming expert.

Translate the following Prolog clause into an equivalent MeTTa rule using S-expressions.

(= (<predicate-name> $arg1 $arg2 ... $result)
   (do-match &self
     "<doc>"
     ...logic...
     (var $result <variable-to-return>)))

Guidelines:
- Use `do-match &self` instead of `do`
- Each clause should represent one type pattern
- Use `(isinstance $x int)`, `(isinstance $x tuple)` guarding clauses
- Avoid nested (if ...) — emit one clause per type
- No nested function calls
- Use `(var $result ..)` instead of `return`
- Output only valid MeTTa — no explanations, no markdown
- One MeTTa clause per Prolog clause

Prolog clause:
{prolog_clause}
"""

def ask_chatgpt(prompt):
    response = client.chat.completions.create(
        model="gpt-4o",
        messages=[{"role": "user", "content": prompt}],
        temperature=0.3
    )
    return response.choices[0].message.content.strip()

def split_predicates(source):
    # Match each top-level predicate or clause ending in a dot
    clauses = re.findall(r'(?:[^.]+?\([^)]*\)[^.]*)\.', source, re.DOTALL)
    return [clause.strip() + '.' for clause in clauses if clause.strip()]

def main():
    if len(sys.argv) != 3:
        print("Usage: python prolog_to_metta.py <input.pl> <output.metta>")
        sys.exit(1)

    input_file, output_file = sys.argv[1], sys.argv[2]

    with open(input_file) as f:
        prolog_code = f.read()

    clauses = split_predicates(prolog_code)

    with open(output_file, 'w') as out:
        for clause in clauses:
            prompt = format_metta_prompt(clause)
            try:
                metta_block = ask_chatgpt(prompt)
                out.write(metta_block + "\n\n")
            except Exception as e:
                print(f"❌ Error converting clause:\n{clause}\n{e}")

    print(f"✅ MeTTa written to {output_file}")

if __name__ == "__main__":
    main()


