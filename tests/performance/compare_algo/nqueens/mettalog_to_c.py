import re

# Mapping of MeTTaLog constructs to C
METTALOG_TO_C = {
    # Comments
    r"^;": r"//",  # Convert comments
    # Function declarations
    r"\(: (\w+) \(-> ([^)]+)\)\)": r"// Function Declaration: \1 :: \2",
    # Function definitions
    r"=\s*\(([\w-]+) ([^()]+)\)": r"\1(\2) {",
    # Basic operators
    r"\(\+= ([^()]+) ([^()]+)\)": r"\1 += \2",
    r"\(-= ([^()]+) ([^()]+)\)": r"\1 -= \2",
    r"\(== ([^()]+) ([^()]+)\)": r"\1 == \2",
    r"\(!= ([^()]+) ([^()]+)\)": r"\1 != \2",
    r"\(< ([^()]+) ([^()]+)\)": r"\1 < \2",
    r"\(> ([^()]+) ([^()]+)\)": r"\1 > \2",
    r"\(<= ([^()]+) ([^()]+)\)": r"\1 <= \2",
    r"\(>= ([^()]+) ([^()]+)\)": r"\1 >= \2",
    r"\(not ([^()]+)\)": r"!(\1)",
    # If statements
    r"\(if ([^()]+) ([^()]+) ([^()]+)\)": r"if (\1) { \2 } else { \3 }",
    # Let bindings (simple transformation)
    r"\(let ([^()]+) ([^()]+)\)": r"{ \1 = \2; }",
    # Function calls
    r"\((\w+) ([^()]+)\)": r"\1(\2);",
    # Closing braces for function definitions
    r"\)": r"}",
}

# Translation function
def translate_mettalog_to_c(mettalog_code):
    c_code = mettalog_code
    for pattern, replacement in METTALOG_TO_C.items():
        c_code = re.sub(pattern, replacement, c_code, flags=re.MULTILINE)
    return c_code

# Example usage
if __name__ == "__main__":
    # Example MeTTaLog Code
    mettalog_code = """
    ; This is a comment
    (: car-atom (-> Atom Atom))
    (: cdr-atom (-> Atom Atom))
    (= (car-atom-or-fail $atom)
       (if (== $atom ()) 
           (let result 0) 
           (car-atom $atom)))
    (= (range $x $y)
       (if (== $x $y)
           $x
           (let z (range (+ $x 1) $y)
               (cons-atom $x $z))))
    """

    print("Original MeTTaLog Code:\n")
    print(mettalog_code)
    
    print("\nTranslated C Code:\n")
    print(translate_mettalog_to_c(mettalog_code))

