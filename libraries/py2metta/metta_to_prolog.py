import sys
from openai import OpenAI

client = OpenAI()

def metta_to_prolog_prompt(metta_code: str) -> str:
    return f"""You are a symbolic AI expert.

Convert the following MeTTa code into idiomatic Prolog.

Rules:
- Convert `(= (func $arg1 $arg2 $out) (match-and ...))` into a predicate like `func(Arg1, Arg2, Out) :- ...`
- Replace MeTTa operators like `(+ $x 1 $y)` with `Y is X + 1`
- Convert `(match-all ...)` to `findall(...)`
- Convert `(not ...)` to `\+ (...)`
- Replace MeTTa lists with standard Prolog list syntax
- Use meaningful variable names like `X`, `Y`, `Result`
- Omit comments and MeTTa-specific formatting
- Output pure Prolog only

MeTTa:
{metta_code}
"""


def ask_chatgpt(prompt: str) -> str:
    response = client.chat.completions.create(
        model="gpt-4o",
        messages=[{"role": "user", "content": prompt}],
        temperature=0.3
    )
    return response.choices[0].message.content.strip()

def convert_metta_to_prolog(metta_code: str) -> str:
    prompt = metta_to_prolog_prompt(metta_code)
    try:
        return ask_chatgpt(prompt)
    except Exception as e:
        print(f"❌ Error converting MeTTa to Prolog: {e}")
        return None

def main():
    if len(sys.argv) != 3:
        print("Usage: python metta_to_prolog.py <input.metta> <output.pl>")
        sys.exit(1)

    with open(sys.argv[1]) as f:
        metta = f.read()

    prolog = convert_metta_to_prolog(metta)

    with open(sys.argv[2], 'w') as f:
        f.write(f"% Converted from MeTTa\n\n{prolog}\n")

    print("✅ Prolog output written to", sys.argv[2])

if __name__ == "__main__":
    main()
