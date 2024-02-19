import os
import json
import ctypes
from ctypes import *
from hyperon.ext import register_atoms
from hyperon import *

class PatternOperation(OperationObject):
    def __init__(self, name, op, unwrap=False, rec=False):
        super().__init__(name, op, unwrap)
        self.rec = rec
    def execute(self, *args, res_typ=AtomType.UNDEFINED):
        return super().execute(*args, res_typ=res_typ)

def wrapnpop(func):
    def wrapper(*args):
        a = [str("'"+arg) if arg is SymbolAtom else str(arg) for arg in args]
        res = func(*a)
        return [res]
    return wrapper

def func_mettamorph(evalstr):
    bytes_literal = evalstr.encode('utf-8')
    input_str = c_char_p(bytes_literal)
    result = mettamorphlib.mettamorph(input_str)
    result_str = ctypes.string_at(result).decode('utf-8')
    return result_str

def quoteSymbol(index, x):
    if index == 0: #first is function symbol
        return x
    if x[0] != "$" and not x.replace("-","").replace(".","").isnumeric() and x!="#f" and x!="#t":
        return ("'" + x).replace("(","(list ").replace(" ", " '")
    return x

def call_mettamorph(*a):
    tokenizer = globalmetta.tokenizer()
    EXPRESSION = "(" + (" ".join([quoteSymbol(i, str(x)).replace("'(","(") for i,x in enumerate(a)])) + ")"
    parser = SExprParser("(superpose " + str(func_mettamorph(EXPRESSION) + ")"))
    return parser.parse(tokenizer)

wrapperfunctions = set([])
def inject_calltypewrapper(content):
    global wrapperfunctions
    wrappers = ""
    for line in content.split("\n"):
        if line.startswith("(= ("):
            argstr = line.split("(= (")[1]
            name = argstr.split(" ")[0]
            i = 0
            args = "("
            counter = 1
            while counter != 0:
                cur = argstr[i]
                args += cur
                if cur == "(":
                    counter += 1
                if cur == ")":
                    counter -= 1
                if counter == 0:
                    break
                i += 1
            wrapperfunction = "(= " + args + " (mettamorph " + name + " " + " ".join(args.split(" ")[1:]) + ")\n"
            if wrapperfunction not in wrapperfunctions:
                wrapperfunctions.add(wrapperfunction)
                wrappers += "(= " + args + " (mettamorph " + name + " " + " ".join(args.split(" ")[1:]) + ")\n"
        if line.startswith("(: "): #TODO multiline typedefs (rare but should work nevertheless)
           wrappers += line + "\n"
    globalmetta.run(wrappers)

compiled = False
def call_compilefile(*a):
    global mettamorphlib, globalmetta, compiled
    if compiled:
        return E(S("Compilation:"), S("unsupported"))
    compiled = True
    loadfiletoken = str(a[0]).replace(") (=", ")\n(=").replace(") (:", ")\n(:")
    loadfile = loadfiletoken[1:-1] if loadfiletoken.startswith('"') or loadfiletoken.startswith('(') else loadfiletoken
    if not loadfile.endswith(".metta"):
        content = loadfile
        loadfile = "TEMP.metta"
        AlreadyWritten = False
        if os.path.exists(loadfile):
            with open(loadfile, "r") as file:
                if content == file.read():
                    AlreadyWritten = True
        if not AlreadyWritten:
            with open(loadfile, "w") as file:
                file.write(content)
    else:
        with open(loadfile, "r") as file:
            content = file.read()
    inject_calltypewrapper(content)
    TEMPfiles = loadfile.replace(".metta", "").upper()
    lastmodification = os.path.getmtime(loadfile)
    status, fcompiles = ("success", "COMPILATIONS.json")
    if os.path.exists(fcompiles):
        with open(fcompiles) as file:
            compilations = json.loads(file.read())
        if loadfile in compilations and compilations[loadfile] == lastmodification:
            status = "skipped"
    else:
        compilations = dict([])
    if status != "skipped":
        cwd = os.getcwd()
        os.chdir("./../")
        os.system(f"sh run_scheme.sh ./extend/{loadfile} cat-only")
        os.chdir(cwd)
        Flags = "-O4" #please see compile_scheme.sh for even better optimization flags!
        os.system(f"cat ./../RUN.scm cinterface.scm > {TEMPfiles}.scm")
        os.system(f'csc {Flags} -DUSE_TYPES {TEMPfiles}.scm cinterface.c -shared || (echo "\nERROR IN TYPE DEFINITIONS ENCOUNTERED!! COMPILING WITHOUT TYPE INFO NOW..." && csc {Flags} {TEMPfiles}.scm cinterface.c -shared)')
        compilations[loadfile] = lastmodification
        with open(fcompiles, 'w') as file:
             file.write(json.dumps(compilations))
    # Load the DLL
    mettamorphlib = ctypes.CDLL(f"{TEMPfiles}.so")
    mettamorphlib.CHICKEN_INIT()
    mettamorphlib.mattamorph_init()
    # Define the argument and return types for the mettamorph function
    mettamorphlib.mettamorph.argtypes = [ctypes.c_char_p]
    mettamorphlib.mettamorph.restype = ctypes.c_char_p
    return E(S("Compilation:"), S(status))

globalmetta = None
@register_atoms(pass_metta=True)
def scheme_atoms(metta):
    global globalmetta
    globalmetta = metta
    with open("../mettamorph.metta") as file:
        metta.run(file.read()) #as "!(import ../mettamorph.metta)" doesn't work here
    call_mettamorph_atom = G(PatternOperation('mettamorph', wrapnpop(call_mettamorph), unwrap=False))
    call_compilefile_atom = G(PatternOperation('compile!', wrapnpop(call_compilefile), unwrap=False))
    return { r"compile!": call_compilefile_atom, r"mettamorph": call_mettamorph_atom }
