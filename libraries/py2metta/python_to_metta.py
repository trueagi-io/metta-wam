import ast
import sys
from openai import OpenAI

client = OpenAI()

def extract_functions(path):
    with open(path) as f:
        tree = ast.parse(f.read())
    return [node for node in tree.body if isinstance(node, ast.FunctionDef)]

def python_to_metta_comment(src):
    lines = src.strip().split('\n')
    return '\n'.join(f";; {line}" for line in lines)

def format_prompt(func):
    src = ast.unparse(func)
    doc = ast.get_docstring(func) or "No documentation provided"
    func_name = func.name
    return f"""You are a symbolic programming expert working in MeTTa.

Translate the following Python function into declarative MeTTa logic.

Follow these rules:

---

✅ OUTPUT FORMAT

Wrap the full function in:

(= ({func_name} $arg1 $arg2 ... $output)
   (match-and &self
     "{doc}"
     ...logic...
     (var $output $result)))

Use logic variables like $x, $y, $out. Do not include Python or Markdown in the output.

---

✅ FOR LOOPS

Translate Python `for` loops using `range` and `tuple-index`.

Example:

Python:
def process_items(xs):
    for i in range(len(xs)):
        process(xs[i])

MeTTa:
(= (process-items $xs $_)
    (match-and &self
        "process each item in xs"
        (length $xs $len)
        (- $len 1 $len-1)
        (match-all 
            "iterate over indices"
            (range 0 $len-1 1 $i)
            (tuple-index $i $xs $item)
            (process $item $_)))) 


---



When you can convert `while` loops into recursive MeTTa relations.

Python:
def process_until_done(xs):
    while not done(xs):
        process(xs) 
MeTTa:
(= (process-until-done $xs $_)
    (match-and &self
        "process until done"
        (not (done $xs))
        (process $xs $_)
        (process-until-done $xs $_)))
    

---

✅ ARRAY / TUPLE INDEXING

Python:
val = xs[i]

MeTTa:
(tuple-index $xs $i $val)

---

✅ MAP + LAMBDA

Use `map` relations and expand `lambda` into named logic.

Python:
doubled = list(map(lambda x: x * 2, items))

MeTTa:
(map $items double-helper $doubled)


(= (double-helper $x $out)
   (match-and &self
     "double each item"
     (* $x 2 $out)))

---

✅ BLOCK SCOPING

Each logic block (loops/ while) must be inside `(match-all ...)`.

but the top level function block will get a `(match-and ...)`.

Conditions will need to be in their own function clauses

---

✅ FUNCTIONS ARE NEVER NESTED

Python:
def ineighbors(loc: IntegerTuple) -> Indices:
     " diagonally adjacent indices "
     return frozenset({{(loc[0] - 1, loc[1] - 1), (loc[0] - 1, loc[1] + 1), (loc[0] + 1, loc[1] - 1), (loc[0] + 1, loc[1] + 1)}})

MeTTa:
(= (ineighbors $loc $indices)
   (match-and &self
     "diagonally adjacent indices"
     (tuple-index $loc 0 $x)
     (tuple-index $loc 1 $y)
     (- $x 1 $x1)
     (- $y 1 $y1)
     (+ $y 1 $y2)
     (+ $x 1 $x2)
     (make-tuple ($x1 $y1) $t1)
     (make-tuple ($x1 $y2) $t2)  
     (make-tuple ($x2 $y1) $t3)
     (make-tuple ($x2 $y2) $t4)
     (make-frozenset ($t1 $t2 $t3 $t4) $indices)))

Python:     
def merge(containers: ContainerContainer) -> Container:
     "" merging ""
     return type(containers)((e for c in containers for e in c))
MeTTa:
(= (merge $containers $result)
   (match-and &self
     "merging"
     (type $containers $container-type)
     (length $containers $len)
     (match-all &self
       "iterate over containers"           
        (range 0 $len 1 $i)
        (tuple-index $containers $i $c)
        (length $c $len-c)
        (match-all &self
            "iterate over elements in container"            
            (range 0 $len-c 1 $j)
            (tuple-index $c $j $e)
            (collect $e $elements)))
        (make-tuple $elements $output)
        (cast $output $container-type $result)))


✅ IF BLOCKS NEED TO DETECT FALLTHROUGHS

Python:

def add(a: Numerical, b: Numerical) -> Numerical:
     "" addition ""
     if isinstance(a, int) and isinstance(b, int):
         return a + b
     elif isinstance(a, tuple) and isinstance(b, tuple):
         return (a[0] + b[0], a[1] + b[1])
     elif isinstance(a, int) and isinstance(b, tuple):
         return (a + b[0], a + b[1])
     return (a[0] + b, a[1] + b)

MeTTa:
(= (add $a $b $output)
   (match-and &self
     "addition"
     (is-int $a)
     (is-int $b)
     (+ $a $b $output)))

(= (add $a $b $output)
   (match-and &self
     "addition"
     (is-tuple $a)
     (is-tuple $b)
     (tuple-index $a 0 $a0)
     (tuple-index $a 1 $a1)
     (tuple-index $b 0 $b0)
     (tuple-index $b 1 $b1)
     (+ $a0 $b0 $out0)
     (+ $a1 $b1 $out1)
     (make-tuple ($out0 $out1) $output)))    

(= (add $a $b $output)
   (match-and &self
     "addition"
     (is-int $a)
     (is-tuple $b)
     (tuple-index $b 0 $b0)
     (tuple-index $b 1 $b1)
     (+ $a $b0 $out0)
     (+ $a $b1 $out1)
     (make-tuple ($out0 $out1) $output)))    

(= (add $a $b $output)
   (match-and &self
     "addition"
     (is-tuple $a)
     (is-int $b)
     (tuple-index $a 0 $a0)
     (tuple-index $a 1 $a1)
     (+ $a0 $b $out0)
     (+ $a1 $b $out1)
     (make-tuple ($out0 $out1) $output)))    

;; Fall through case
(= (add $a $b $output) 
    (match-and &self
      "addition"
      (not (is-int $a))
      (not (is-tuple $a))
      (not (is-int $b))
      (not (is-tuple $b))
      (tuple-index $a 0 $a0)
      (tuple-index $a 1 $a1)
      (+ $a0 $b $out0)
      (+ $a1 $b $out1)
      (make-tuple ($out0 $out1) $output)))    
     

Python:
 def leastcolor(element: Element) -> Integer:
     "" least common color ""
     values = [v for r in element for v in r] if isinstance(element, tuple) else [v for v, _ in element]
     return min(set(values), key=values.count)

MeTTa:
(= (leastcolor $element $output)
   (match-and &self
     "least common color"
     (is-tuple $element)
     (length $element $len)
     (match-all &self
       "iterate over rows"
       (range 0 $len 1 $i)
       (tuple-index $element $i $r)
       (length $r $len-r)
       (match-all &self
         "iterate over values in row"
         (range 0 $len-r 1 $j)
         (tuple-index $r $j $v)
         (collect $v $values))))
     (not (is-tuple $element))
     (length $element $len)
     (match-all &self
       "iterate over element pairs"
       (range 0 $len 1 $i)
       (tuple-index $element $i $pair)
       (tuple-index $pair 0 $v)
       (collect $v $values)))
     (make-set $values $unique-values)
     (length $unique-values $unique-len)
     (match-all &self
       "find least common color"
       (range 0 $unique-len 1 $k)
       (tuple-index $unique-values $k $val)
       (count $values $val $count)
       (make-tuple ($count $val) $count-val)
       (collect $count-val $count-values))
     (min-by $count-values first $min-count-val)
     (tuple-index $min-count-val 1 $output)))

Python:
def switch(grid: Grid, a: Integer, b: Integer) -> Grid:
     "" color switching ""
     return tuple((tuple((v if v != a and v != b else {{a: b, b: a}}[v] for v in r)) for r in grid))

MeTTa:
(= (switch $grid $a $b $output)
   (match-and &self
     "color switching"
     (length $grid $len)
     (match-all &self
       "iterate over rows"
       (range 0 $len 1 $i)
       (tuple-index $grid $i $r)
       (length $r $len-r)
       (match-all &self
         "iterate over values in row"
         (range 0 $len-r 1 $j)
         (tuple-index $r $j $v)
         (match-and &self
           "check value"
           (== $v $a $new-v)
           (== $v $b $new-v)
           (not new-v)
           (collect $v $new-row))
         (match-and &self
           "switch value"
           ;; (or (= $v $a) (= $v $b))
           (== $v $a $new-v)
              (== $v $b $new-v2)
              (or $new-v1 $new-v2)
              (make-tuple ($a $b) ($b $a) $switch-map)
                (tuple-index $switch-map $new-v $new-v1)
                (collect $new-v1 $new-row)) 
            (make-tuple $new-row $new-r)))
        (make-tuple $new-r $output)))



✅ IMPORTING LIBRARIES
- use MeTTa's built-in `!(import! package::module)` syntax to import


✅ WHAT TO OUTPUT
Output only valid MeTTa code — no explanation, no markdown.

```python
{src}
```
""".strip()

def ask_chatgpt(prompt):
    response = client.chat.completions.create(
        model="gpt-4o",
        messages=[{"role": "user", "content": prompt}],
        temperature=0.3
    )
    return response.choices[0].message.content.strip()

def convert_python_to_metta(src):
    prompt = format_prompt(src)
    try:
        metta_code = ask_chatgpt(prompt)
        return metta_code
    except Exception as e:
        print(f"❌ Error converting function {src.name}: {e}")
        return None

def run_second_pass(metta_code: str) -> str:
    prompt = f"""You are a MeTTa expert.

Review the following MeTTa code. Your task is to corect a previous pass that did not finish the job.


- Extract intermediate expressions into separate statements using temporary variables appened to the function (code only gets longer.  Dont leave anything out or optimize it.)

Examples of a nested function calls:
```metta
(= (hconcat $a $b $output)
   (match-and &self
     "concatenate two grids horizontally"
     (length $a $len)
     (match-all &self
       "iterate over rows"
       (range 0 $len 1 $i)
       (tuple-index $a $i $row-a)       
       (concat $row-a (tuple-index $b $i) $new-row)
       (collect $new-row $new-rows))
     (make-tuple $new-rows $output)))

(= (dmirror $piece $output)
   (match-and &self
     "mirroring along diagonal"
     (not (is-tuple $piece))
     (ulcorner $piece $a $b)
     (next-iter $piece $first)
     (tuple-index $first 1 $first-inner)
     (is-tuple $first-inner)
     (length $piece $len)
     (match-all &self
       "iterate over piece elements"
       (range 0 $len 1 $i)
       (tuple-index $piece $i $pair)
       (tuple-index $pair 0 $v)
       (tuple-index $pair 1 $coords)
       (tuple-index $coords 0 $i-coord)
       (tuple-index $coords 1 $j)
       (- $j $b $j-b)
       (+ $j-b $a $new-j)
       (- $i-coord $a $i-a)
       (+ $i-a $b $new-i)
       (make-tuple ($v (make-tuple ($new-j $new-i))) $new-pair)
       (collect $new-pair $new-pairs))
     (make-frozenset $new-pairs $output)))

```
You should rewrite it to:
```metta
(= (hconcat $a $b $output)
   (match-and &self
     "concatenate two grids horizontally"
     (length $a $len)
     (match-all &self
        "iterate over rows"
        (range 0 $len 1 $i)
        (tuple-index $a $i $row-a)
        (tuple-index $b $i $row-b) ;; Extracted tuple-index into a separate statement
        (concat $row-a $row-b $new-row) ;; Use the extracted variable         
        (collect $new-row $new-rows))
     (make-tuple $new-rows $output)))

(= (dmirror $piece $output)
   (match-and &self
     "mirroring along diagonal"
     (not (is-tuple $piece))
     (ulcorner $piece $a $b)
     (next-iter $piece $first)
     (tuple-index $first 1 $first-inner)
     (is-tuple $first-inner)
     (length $piece $len)
     (match-all &self
       "iterate over piece elements"
       (range 0 $len 1 $i)
       (tuple-index $piece $i $pair)
       (tuple-index $pair 0 $v)
       (tuple-index $pair 1 $coords)
       (tuple-index $coords 0 $i-coord)
       (tuple-index $coords 1 $j)
       (- $j $b $j-b)
       (+ $j-b $a $new-j)
       (- $i-coord $a $i-a)
       (+ $i-a $b $new-i)
       (make-tuple ($new-j $new-i) $new-coords) ;; Extracted new coordinates into a separate statement
       (make-tuple ($v $new-coords) $new-pair) ;; Use the extracted variable
       (collect $new-pair $new-pairs))
     (make-frozenset $new-pairs $output)))

```

Also sometimes the previous pass forgot to break up if/then statements into separate statements, so you should do that too.
You can tell on this one because $output is attemting to be assigned multiple times in the same function.

```metta
(= (crement $x $output)
   (match-and &self
     "incrementing positive and decrementing negative"
     (is-int $x)
     (match-and &self
       "check if x is zero"
       (== $x 0)
       (var $output 0))
     (match-and &self
       "check if x is positive"
       (> $x 0)
       (+ $x 1 $output))
     (match-and &self
       "check if x is negative"
       (< $x 0)
       (- $x 1 $output))))
```

Needs to become:
```metta
;; $x is int and zero (became a separate statement)
(= (crement $x $output)
   (match-and &self
     "incrementing positive and decrementing negative"
     (is-int $x)
     (match-and &self
       "check if x is zero"
       (== $x 0)
       (var $output 0)))
;; $x is int and positive (became a separate statement)
(= (crement $x $output)
   (match-and &self
     "incrementing positive and decrementing negative"
     (is-int $x)
        (match-and &self
         "check if x is positive"
            (> $x 0)
            (+ $x 1 $output)))
;; $x is int and negative (became a separate statement)
(= (crement $x $output)
   (match-and &self
     "incrementing positive and decrementing negative"
     (is-int $x)
        (match-and &self
         "check if x is negative"
            (< $x 0)
            (- $x 1 $output)))  
```




Here is the MeTTa code:

{metta_code}

Return only the cleaned MeTTa code with commented changes.
"""
    response = client.chat.completions.create(
        model="gpt-4o",
        messages=[{"role": "user", "content": prompt}],
        temperature=0.3
    )
    return response.choices[0].message.content.strip()

def main():
    if len(sys.argv) != 3:
        print("Usage: python python_to_metta.py <input.py> <output.metta>")
        sys.exit(1)

    input_file, output_file = sys.argv[1], sys.argv[2]
    functions = extract_functions(input_file)

    with open(output_file, 'w') as f:
        f.write(f";; MeTTa code generated from {input_file}\n\n")

    for func in functions:
        python_src = ast.unparse(func)
        comment = python_to_metta_comment(python_src)

        metta_code = convert_python_to_metta(func)
        if not metta_code:
            print(f"❌ Failed to convert function {python_src}. Skipping...")
            continue    
        # If conversion failed, skip to the next function
        if not isinstance(metta_code, str):
            print(f"❌ Invalid MeTTa code for function {python_src}. Skipping...")
            continue


        better_metta_code = run_second_pass(metta_code)
        if not better_metta_code:
            print(f"❌ Failed to clean up MeTTa code for function {python_src}. Skipping...")
            better_metta_code = metta_code  # Fallback to original if cleanup fails

        block = f"{comment}\n/*\n{metta_code} */\n{better_metta_code}\n"        
        print("🔹 Generated MeTTa block:\n")
        print(block)

        blockSave = f"{comment}\n{better_metta_code}\n"
        with open(output_file, 'a') as f:
            f.write(blockSave + "\n")

if __name__ == "__main__":
    main()
