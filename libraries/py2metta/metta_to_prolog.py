#LEFT
import sys
import time
from datetime import datetime
import os
import argparse
import re
from openai import OpenAI

# Global control state and translation context
client = OpenAI()
log_path = None
next_verifier_context = ""
next_translator_advice = ""

# Translation rules (static reference)
TRANSLATION_RULES = """
% --- translation rules ---
- Convert `(= (func $arg1 $arg2 $out) (match-and ...))` into a predicate like `func(Arg1, Arg2, Out) :- ...`
- Replace MeTTa operators like `(+ $x 1 $y)` with `Y is X + 1`
- Convert `(match-all ...)` to `findall(...)`
- Convert `(not ...)` to `\+ (...)`
- Replace MeTTa lists with standard Prolog list syntax
- Use meaningful variable names like `X`, `Y`, `Result`
- Include comments from MeTTa and yourself
- Output pure Prolog only
- Do not omit or summarize any logic
- Translate every part of the MeTTa code in full
- Helper predicates such as `append/3`, `last/2`, `metta_equal/2`, `as_tf/2`, etc., are defined elsewhere
- as_tf: `as_tf(Goal,TF):- Goal*->TF='True';TF='False'.`
- `match &self (pattern ...) (body ...)`, we use Prolog's pattern-matching and backtracking.
- `let`, `let*`, and `progn`, we use local variables and sequential goals.
- `collapse (match ...)`, we use `findall/3`.
- `case`, we use Prolog's `;` and pattern matching.
- `if`, we use `-> ;`.
- `superpose`, we use list membership or backtracking.
- `(empty)` and `Empty`, we use prolog failure.
- `True`/`False`, we use atoms `'True'`/`'False'`.
- `size-atom`, we use `length/2`.
- `car-atom`, `cdr-atom`, we use `[H|T]` and `T`.
- `index-atom`, we use `nth1/3` (1-based, 1st/2nd arg swapped).
- `cons-atom`, we use `[H|T]`.
- `format-args` and `println!`, we use `format/2` or `writeln/1`.
- `(random-int &rng 1 N)`, we use `random_between(1,N,Out)`.
- `add-atom`, `remove-atom`, etc., we assume dynamic predicates and `assertz/1`, `retractall/1`, etc.
- These are structural mappings from MeTTa to Prolog syntax.
 - `(= (is-expression $x) (== (get-metatype $x) Expression))`
     → `is_expression(X, TF) :- get_metatype(X,MT), as_tf(metta_equal(MT, 'Expression'), TF).`
 - `(= (add-one $x) (+ $x 1))`
     → `add_one(X, Y) :- Y is X + 1.`
 - `(= (is-even $n) (== (mod $n 2) 0))`
     → `is_even(N, TF) :- as_tf((0 is N mod 2), TF).`
 - `(= (foo $x $y) (== $x $y))`
     → `foo(X, Y, TF) :- as_tf(metta_equal(X,Y), TF).`
 - `(= (member $x $list) (match-all $list (cons $head $tail) (or (== $x $head) (member $x $tail))))`
     → `member(X, [Head|Tail]) :- unifies(X, Head). member(X, [_|Tail]) :- member(X, Tail).`
"""

# Model limits and fallback size
MODEL_CONTEXT_LIMITS = {
    "codex-mini-latest": 18000,
    "gpt-4.1": 1000000,
    "gpt-4.1-mini": 1000000,
    "gpt-4.1-nano": 1000000,
    "gpt-4-turbo": 1000000,
    "gpt-4o": 128000,
    "gpt-4o-mini": 128000,
    "o3": 200000,
    "o3-pro": 200000,
    "o3-mini": 200000,
    "o4-mini": 200000,
    "o1": 18000,
    "o1-pro": 18000,
    "gpt-3.5-turbo": 16384,
    "babbage-002": 8192,
    "davinci-002": 8192
}

FALLBACK_TOKENS = 18000


# ANSI escape color codes for consistent styling
GRAY = "\033[90m"
BLACK   = "\033[30m"
RED     = "\033[31m"
GREEN   = "\033[32m"
YELLOW  = "\033[33m"
BLUE    = "\033[34m"
MAGENTA = "\033[35m"
CYAN    = "\033[36m"
WHITE   = "\033[37m"
LIGHT_MAGENTA = "\033[95;1m"
LIGHT_GREEN   = "\033[92;1m"

RESET     = "\033[0m"
BOLD      = "\033[1m"
DIM       = "\033[2m"
ITALIC    = "\033[3m"
UNDERLINE = "\033[4m"
BLINK     = "\033[5m"
REVERSE   = "\033[7m"
HIDDEN    = "\033[8m"
STRIKETHROUGH = "\033[9m"




def preview_chunk(label, icon, color, content):
    line_count = len(content.splitlines())
    char_count = len(content)
    print(BOLD + "─" * 120)
    print(f"{icon} {label} | {line_count} lines | {char_count} chars")
    print(BOLD + "─" * 120 + RESET)
    print(color + content.strip() + RESET + "\n")
    print(BOLD + "─" * 120)
    print(f"{icon} End of {label} | {line_count} lines | {char_count} chars")
    print(BOLD + "─" * 120 + RESET + "\n")

def log_exchange(label, content):
    if not log_path:
        return
    with open(log_path, "a") as f:
        f.write(f"\n===== {label} =====\n")
        f.write(content.strip())
        f.write("\n====================\n")

def embed_conversion_metadata(block_num, total_blocks, original_metta, previous_prolog, input_name, next_translator_advice):
    original_metta_comment = original_metta.replace('\n', '\n% ')
    previous_prolog_comment = previous_prolog.replace('\n', '\n% ')
    translation_advice_comment = next_translator_advice.replace('\n', '\n% ')

    metadata = f"""% --- GPT Conversion Metadata ---
% block_num: {block_num}/{total_blocks}
% input_name: {input_name}
% original_metta: |
% {original_metta_comment}
% previous_prolog: |
% {previous_prolog_comment}
% --- End of Metadata ---

% --- conversion advice ---
% {translation_advice_comment}

% --- converted to prolog ---
% (GPT should fill in the translated Prolog code here)

% --- notes for user ---
% (GPT should summarize any corrections or improvements here)

% --- notes for next translator (function signatures defined) ---
% (GPT should list function signatures and translation strategies here)
"""
    return metadata

def extract_metadata_notes(block_text):
    notes_for_user = ""
    notes_for_next_translator = ""

    matches = re.findall(r"% --- notes for user.*?---", block_text, flags=re.DOTALL)
    if matches:
        notes_for_user = matches[0].split("---")[1].strip()

    matches = re.findall(r"% --- notes for next translator.*?---", block_text, flags=re.DOTALL)
    notes_for_next_translator = matches[0].split("---")[1].strip()

    return notes_for_user, notes_for_next_translator

def metta_to_prolog_prompt(metta_code: str, prompt_Ltext: str, prompt_name: str, model: str, model_context: int) -> str:
    prompt_text = f"""You are a symbolic AI expert translating MeTTa to Prolog.

{prompt_Ltext.strip()}

Instructions:
- Fill in the "% --- notes for next translator ---" section with updated function signatures and naming conventions.
% (GPT should list function signatures and translation strategies here)
- For the section starting with "% --- notes for user ---", include:
% (GPT should summarize any corrections or improvements here)
  - Always summarize the type of correction (syntax, logic, renaming, etc).
  - Optionally annotate function signatures to help the next translator.
- You are part of a multi-step translation process.
- A future GPT may review your work (but you are NOT to assume they will).
- Most importantly: Leave a clear message for the *next translator*:
  - Include all function signatures you defined
  - List any translation patterns, naming conventions, or partials you left unfinished
  - Use the standard metadata template:
    % --- converted to prolog ---
% (GPT should fill in the translated Prolog code here)
    % --- notes for user ---
% (GPT should summarize any corrections or improvements here)
    % --- notes for next translator (function signatures defined) ---
% (GPT should list function signatures and translation strategies here)
- Do NOT change or discard metadata sections — future GPTs need them intact.

MeTTa:
{metta_code.strip()}"""
    return prompt_text

def ask_chatgpt(prompt: str, model: str, temperature: float):
    start_time = time.time()
    response = client.chat.completions.create(
        model=model,
        messages=[{"role": "user", "content": prompt}],
        temperature=temperature
    )
    duration = time.time() - start_time
    return response.choices[0].message.content.strip(), duration

def verify_prolog_output(original_metta, generated_prolog, prompt_text, model, temperature, very_verbose=False):
    start_time = time.time()
    verifier_prompt = f"""
You are a symbolic AI expert and Prolog translator reviewing output from a prior GPT.

Instructions:
  - Your job is not just to check Prolog syntax.
  - You must verify that the Prolog code is a correct and complete translation of the original MeTTa block.
  - Check:
- All MeTTa logic was translated
- Translation follows the `% --- translation rules ---`
- No parts were skipped, altered, or loosely rephrased
- Leave your review inside the metadata sections.
- For the section starting with "% --- notes for user ---", include:
% (GPT should summarize any corrections or improvements here)
  - Always summarize the type of correction (syntax, logic, renaming, etc).
  - Optionally annotate function signatures to help the next translator.
  - Leave detailed notes for the user in the "% --- notes for user ---" section, especially if you made any corrections.
% (GPT should summarize any corrections or improvements here)
- Your task is to verify and correct the Prolog code translated from MeTTa.
- Fix only logical or syntactic mistakes.
- Do NOT summarize or shorten any logic — expand if necessary.
- Preserve variable naming conventions unless they are ambiguous or incorrect.
- You MUST output the full structured metadata block with updates to:
  - % --- converted to prolog ---
% (GPT should fill in the translated Prolog code here)
  - % --- notes for next translator (function signatures defined) ---
% (GPT should list function signatures and translation strategies here)
If the previous Prolog is correct, copy it into the correct metadata field without change.

This verification is part of a multi-step pipeline.


{TRANSLATION_RULES}\n\n--- MeTTa ---
{original_metta.strip()}

--- Prolog (from another model) ---
{generated_prolog.strip()}

{prompt_text.strip()}
"""
    if very_verbose:
        preview_chunk(f"Sent to {model} for verification (temp={temperature})", "📤", LIGHT_MAGENTA, verifier_prompt)
    try:
        response = client.chat.completions.create(
            model=model,
            messages=[{"role": "user", "content": verifier_prompt}],
            temperature=temperature
        )
        content = response.choices[0].message.content.strip()
        duration = time.time() - start_time
        notes_for_user, notes_for_next_translator = extract_metadata_notes(content)
        global next_translator_advice, next_verifier_context
        next_verifier_context = notes_for_user
        next_translator_advice = notes_for_next_translator
        log_exchange(f"Received from {model} (verifier) in {duration:.2f}s", content)
        if very_verbose:
            preview_chunk(f"Received from {model} (verification) in {duration:.2f}s", "📥", LIGHT_GREEN, content)
        return content
    except Exception as e:
        print(f"⚠️ Verification failed with {model}: {e}", file=sys.stderr)
        return generated_prolog
    except Exception as e:
        print(f"⚠️ Verification failed with {model}: {e}", file=sys.stderr)
        return generated_prolog

def convert_block(metta_block, input_name, prompt_text, model, model_context, temperature, block_num, total_blocks, next_translator_advice, very_verbose=False):
    prompt = metta_to_prolog_prompt(metta_block, prompt_text, input_name, model, model_context)
    start_time = time.time()
    log_exchange( f"Sent to {model}", prompt)
    if very_verbose:
        preview_chunk(f"Sent to {model} (temp={temperature})", "📤", "[95m", prompt)
    prolog_output, duration = ask_chatgpt(prompt, model=model, temperature=temperature)
    duration = time.time() - start_time
    log_exchange( f"Received from {model}", prolog_output + f" in {duration:.2f}s")
    if very_verbose:
        preview_chunk(f"Received from {model} in {duration:.2f}s", "📥", "[92m", prolog_output + f" in {duration:.2f}s")

    metadata = embed_conversion_metadata(block_num, total_blocks, metta_block, prolog_output, input_name, next_translator_advice)
    return f"{metadata}\n{prolog_output}"
    
def extract_balanced_blocks(text: str, min_lines=2, min_chars=None):
    return extract_blocks_with_soft_min(text, min_lines, min_chars)

def extract_blocks_with_soft_min(text: str, min_lines=1, min_chars=0):
    lines = text.splitlines()
    block = []
    char_count = 0
    cut_pending = False
    i = 0

    last_comment_idx = None  # ← Track where the comment block starts

    def is_blank(line):
        return len(line.strip()) == 0

    def is_line_boundary(line):
        head = line[:6].lstrip()
        chead = line[:3].lstrip()
        return head.startswith("(= ") or head.startswith("!(") or chead.startswith(";") or head.startswith("(: ")

    def is_comment_line(line):
        return line[:6].lstrip().startswith(";")

    while i < len(lines):
        raw_line = lines[i]

        # Normalize whitespace for parsing (but keep raw_line in block)
        line = raw_line.replace('\t', '     ')
        line = re.sub(r'\(\s+', '(', line)

        # Track comment block start
        if is_comment_line(line):
            if last_comment_idx is None:
                last_comment_idx = len(block)

        elif not is_blank(line):
            last_comment_idx = None  # exited comment block

        block.append(raw_line)
        char_count += len(raw_line) + 1

        if not cut_pending and char_count >= min_chars:
            cut_pending = True

        elif cut_pending and is_line_boundary(line) and not is_blank(line):
            if last_comment_idx is not None:
                # We're still in a comment block → yield up to its start
                yield '\n'.join(block[:last_comment_idx]).strip()
                block = block[last_comment_idx:]
                char_count = sum(len(ln) + 1 for ln in block)
                last_comment_idx = None
            else:
                # Safe to yield just before this line
                yield '\n'.join(block[:-1]).strip()
                block = [raw_line]
                char_count = len(raw_line) + 1
            cut_pending = False

        i += 1

    if block and len(block) >= min_lines:
        yield '\n'.join(block).strip()

def main():
    parser = argparse.ArgumentParser(
        description="Convert MeTTa code to Prolog (chunked, append-mode) and optionally verify with a second model.",
        usage="python metta_to_prolog.py -i <input.metta> -o <output.pl> [-p <prompt.txt>] [-m gpt-4.1] [-mv gpt-4o] [-t 0.3]"
    )
    model_choices = list(MODEL_CONTEXT_LIMITS.keys())
    model_help = (
        "Primary OpenAI model to use (default: gpt-4.1). "
        "Known options: " + ", ".join(sorted(MODEL_CONTEXT_LIMITS.keys())) +
        ". \r Unrecognized models allowed with fallback context = " + str(FALLBACK_TOKENS) + " tokens."
    )

    parser.add_argument("-i", "--input", required=True, help="Input MeTTa file (or '-' for stdin)")
    parser.add_argument("-o", "--output", required=True, help="Output Prolog file (or '-' for stdout)")
    parser.add_argument("-p", "--prompt", help="Extra prompt file to prepend to GPT input", default=None)
    parser.add_argument("-m", "--model", help=model_help, default="gpt-4.1")
    parser.add_argument("-mv", "--mverify", help="Optional second model to verify and correct Prolog output", default="gpt-4.1")
    parser.add_argument("-t", "--temperature", type=float, help="Sampling temperature", default=0.3)
    parser.add_argument("--preview", action="store_true", help="Preview extracted MeTTa blocks without calling GPT")    
    parser.add_argument("--preview-blocks", action="store_true", help="Print block boundaries and preview content without converting")
    parser.add_argument("-trans", "--translation", type=str, default="default")
    parser.add_argument("--list-translations", action="store_true", default=False)
    parser.add_argument("-mt", "--max-tokens", type=int, help="Override token window for chunk sizing")
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose", default=False)
    parser.add_argument("-vv", "--very-verbose", action="store_true", help="Very verbose (show GPT input/output)", default=False)
    parser.add_argument("--dump-state", action="store_true", help="Show next_translator_advice and next_verifier_context")
    parser.add_argument("--chunk", type=int, help="Override chunk size in characters")
    parser.add_argument("--no-verify", action="store_true", help="Disable verifier pass entirely")
    args = parser.parse_args()
    if args.dump_state:
        print("\n📎 next_translator_advice:\n", next_translator_advice.strip())
        print("\n🕵️ next_verifier_context:\n", next_verifier_context.strip())
    global log_path
    log_path = args.output + ".log"
    conversion_errors = 0

    input_name = "stdin" if args.input == "-" else os.path.basename(args.input)
    metta_code = "".join(sys.stdin) if args.input == "-" else open(args.input).read()
    prompt_text = open(args.prompt).read() if args.prompt else ""

    model_context = MODEL_CONTEXT_LIMITS.get(args.model, FALLBACK_TOKENS)
    token_window = args.max_tokens if args.max_tokens else model_context
    chars_min = args.chunk if args.chunk else int(token_window * 0.1 * 4)
    print("chars_min = ", chars_min)

    if args.preview_blocks:
        print("🔍 Previewing detected blocks:\n")
        for idx, block in enumerate(extract_balanced_blocks(metta_code, min_lines=2, min_chars=chars_min), start=1):
            preview_chunk(f"Block {idx}", "🧩", "\033[94m", block)
        print("\n✅ End of preview.")
        return

    if args.verbose:
        print(f"📐 Model '{args.model}' allows ~{model_context} tokens (~{model_context*4:,} chars)")
        print(f"🔢 Enforcing 10% safety chunk size: {chars_min:,} characters")

    if len(metta_code) > chars_min:
        print(f"⚠️ Input exceeds 10% of model context window ({len(metta_code):,} chars > {chars_min:,}).")
        print(f"🔀 Splitting input into safe-sized chunks for conversion...")

    if args.output != "-":
        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        with open(args.output, "w") as f:
            f.write(f"/* Prolog generated from MeTTa source: {input_name}\n")
            f.write(f"   Timestamp: {now} */\n\n")

    total_blocks = sum(1 for _ in extract_balanced_blocks(metta_code, min_lines=20, min_chars=chars_min))
    for block_num, block in enumerate(extract_balanced_blocks(metta_code, min_lines=20, min_chars=chars_min), start=1):
        
        if args.preview or args.verbose:
            preview_chunk("MeTTa Block", "🧱", YELLOW, f"/* Converted from MeTTa:\n{block}\n*/")

        prolog_code = convert_block(block, input_name, prompt_text, args.model, model_context, args.temperature, block_num, total_blocks, next_translator_advice, very_verbose=args.very_verbose)
        verified = prolog_code
        if args.mverify and not args.no_verify:
            verified = verify_prolog_output(block, prolog_code, prompt_text, args.mverify, args.temperature, very_verbose=args.very_verbose)

        if args.preview or args.verbose:            
            if args.mverify and verified != prolog_code:
                preview_chunk("Pre-verified Prolog", "🧩", GREEN, f"/* Pre-verified Prolog (from model: {args.model}):\n{prolog_code}\n*/")
            preview_chunk("Final Prolog Output", "📜", CYAN, verified)
            if args.preview:
                continue

        if not verified:
            conversion_errors += 1
            print("⚠️ Skipped due to conversion error.")
            continue

        if args.output == "-":
            sys.stdout.write(f"/* Converted from MeTTa:\n{block}\n*/\n\n")
            if args.mverify and verified != prolog_code:
                sys.stdout.write(f"/* Pre-verified Prolog (from model: {args.model}):\n{STRIKETHROUGH}{prolog_code}\n*/\n\n")
            sys.stdout.write(f"{verified}\n\n")
        else:
            with open(args.output, "a") as f:
                f.write(f"/* Converted from MeTTa:\n{block}\n*/\n\n")
                if args.mverify and verified != prolog_code:
                    f.write(f"/* Pre-verified Prolog (from model: {args.model}):\n{prolog_code}\n*/\n\n")
                f.write(f"{verified}\n\n")

    if args.output != "-":
        with open(args.output, "a") as f:
            f.write(f"/* Conversion complete. {conversion_errors} block(s) failed to convert. */\n")
        print(f"✅ Appended Prolog to {args.output}")

if __name__ == "__main__":
    main()


