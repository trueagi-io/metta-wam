from hyperon.base import Atom
from hyperon.atoms import OperationAtom, E
from hyperon.ext import register_tokens
from hyperon.ext import register_atoms
from hyperon.atoms import G, AtomType
from hyperon.runner import MeTTa

import hyperonpy as hp

import sys
import readline
import os
import atexit
import traceback

# Module-level verbosity levels
SILENT = 0  # No output
USER = 1    # Basic output for users
DEBUG = 2   # Detailed debug information
TRACE = 3   # Granular trace-level output

# Default verbosity level
METTALOG_VERBOSE = USER

# History file for REPL
histfile = os.path.join(os.path.expanduser("~"), ".metta_history")

try:
    readline.set_history_length(10000)
    readline.read_history_file(histfile)
    h_len = readline.get_current_history_length()
except FileNotFoundError:
    open(histfile, 'wb').close()
    h_len = 0

def save_history(prev_h_len, histfile):
    """Save command history to the history file."""
    new_h_len = readline.get_current_history_length()
    readline.set_history_length(10000)
    readline.append_history_file(new_h_len - prev_h_len, histfile)

atexit.register(save_history, h_len, histfile)

# Set verbosity level
def set_verbosity(level):
    global METTALOG_VERBOSE
    if level in [SILENT, USER, DEBUG, TRACE]:
        METTALOG_VERBOSE = level
        log_message(USER, f"Verbosity set to level {level}")
    else:
        print(f"Invalid verbosity level '{level}' provided. Defaulting to USER level.")
        METTALOG_VERBOSE = USER

# Log messages based on verbosity level
def log_message(level, message):
    if METTALOG_VERBOSE >= level:
        print(message)

# Manual command-line argument processing in real-time
def process_args(runner):
    wont_need_repl = False  # Keep track if we won't need to start the REPL
    i = 1  # Skip the first argument (script name)

    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg in ("-v", "--verbosity"):
            try:
                verbosity_level = int(sys.argv[i + 1])
                set_verbosity(verbosity_level)
                i += 1  # Skip next item
            except (ValueError, IndexError):
                print("Invalid verbosity level. Defaulting to USER.")
                set_verbosity(USER)

        elif arg in ("-p", "--path"):
            if i + 1 < len(sys.argv):
                path = sys.argv[i + 1]
                log_message(DEBUG, f"Adding path: {path}")
                runner.add_path(path)
                i += 1

        elif arg in ("-m", "--module"):
            if i + 1 < len(sys.argv):
                module = sys.argv[i + 1]
                log_message(DEBUG, f"Adding module: {module}")
                runner.add_module(module)
                i += 1

        elif arg in ("-l", "--library"):
            if i + 1 < len(sys.argv):
                library = sys.argv[i + 1]
                log_message(DEBUG, f"Adding library: {library}")
                runner.add_library(library)
                i += 1

        elif arg in ("-f", "--file"):
            if i + 1 < len(sys.argv):
                file = sys.argv[i + 1]
                load_file(runner, file)
                wont_need_repl = True  # Mark that a file was loaded
                i += 1

        elif arg == "--repl":
            # Start REPL immediately and ignore further arguments
            REPL(runner).main_loop()

        elif arg == "--version":
            log_message(USER, f"Hyperon version: {hyperon.__version__}")
            sys.exit(0)  # Exit after showing version

        elif arg in ("-h", "--help"):        
            print_help()
            sys.exit(0)  # Exit after showing help

        else:
            # Assume this is a file to process
            load_file(runner, arg)
            wont_need_repl = True  # Mark that a file was loaded

        i += 1

    return wont_need_repl

# Print help information
def print_help():
    # Get the script name dynamically
    script_name = os.path.basename(sys.argv[0])  # This will get the current script's name

    help_text = f"""
MeTTaLog-REPL in Python

Usage: {script_name} [options] [files]

Options:
  -v, --verbosity <level>   Set verbosity level: 0 (SILENT), 1 (USER), 2 (DEBUG), 3 (TRACE)
  -p, --path <path>         Add search paths for modules
  -m, --module <module>     Add modules to the runner
  -l, --library <library>   Add libraries to the runner
  -f, --file <file>         MeTTa script files to execute
  --repl                    Start REPL after processing files
  --version                 Print the version and exit
  --help                    Display this help message
    """
    print(help_text)

# Load and execute a file
def load_file(runner, file):
    log_message(DEBUG, f"Executing MeTTa script from: {file}")
    try:
        with open(file) as f:
            program = f.read()
        for result in runner.run(program):
            log_message(USER, f"Result: {result}")
    except FileNotFoundError:
        log_message(USER, f"Error: File '{file}' not found.")
    except Exception as e:
        log_message(USER, f"An error occurred: {e}")
        if METTALOG_VERBOSE >= DEBUG:
            traceback.print_exc()

class MeTTaVS(MeTTa):

    def __init__(self, cmetta = None, space = None, env_builder = None):
        super().__init__(cmetta=cmetta, space=space,env_builder=env_builder)
    def copy(self):
        return self
    def run(self, program, flat=False):
        """Runs the MeTTa code from the program string containing S-Expression MeTTa syntax"""
        import hyperon
        startWithBang = False
        if str(program).startswith('!'):
            program = program[1:]
            startWithBang = True

        from hyperon.atoms import Atom
        parser = hyperon.base.SExprParser(program)
        if startWithBang:
            atom = parser.parse(self.tokenizer())
            result = hp.metta_evaluate_atom(self.cmetta, atom.catom)
            return [Atom._from_catom(catom) for catom in result]
        else:
            results = hp.metta_run(self.cmetta, parser.cparser)
            self._run_check_for_error()
            if flat:
                return [Atom._from_catom(catom) for result in results for catom in result]
            else:
                return [[Atom._from_catom(catom) for catom in result] for result in results]
    def evaluate_atom(self, atom):
        import hyperon

        from hyperon.atoms import Atom
        result = hp.metta_evaluate_atom(self.cmetta, atom.catom)
        self._run_check_for_error()
        return [Atom._from_catom(catom) for catom in result]


global runner, runnerAtom
runnerAtom = None
runner = None

@register_atoms
def my_imported_runner_atom():
    global runner, runnerAtom
    # We don't use metta here, but we could...
    content = '''
        (: fact (-> Number Number))
        (= (fact $x)
           (case $x
            ((0 1)
             ($_ (* $x (fact (- $x 1)))))
           )
        )

        (some content)
        (= (self-from-self)
           (match &self (some $x) $x))

        something

        (= (call_func $f $arg) ($f $arg))
    '''    
    runner.run(content)
    
    return {
        'r': runnerAtom
    }

@register_tokens(pass_metta=True)
def my_resolver_atoms(metta):

    def run_resolved_symbol_op(runner, atom, *args):
        expr = E(atom, *args)
        result = hp.metta_evaluate_atom(runner.cmetta, expr.catom)
        result = [Atom._from_catom(catom) for catom in result]
        return result

    def resolve_atom(metta, token):
        # TODO: nested modules...
        runner_name, atom_name = token.split('::')
        # FIXME: using `run` for this is an overkill,
        #        but there is no good Python API for this;
        #        we may have an interface function for
        #        `tokenizer` to resolve individual symbols -
        #        metta.tokenizer().find_token ...
        #        or something else...
        # TODO: assert
        runner = metta.run('! ' + runner_name)[0][0].get_object()
        atom = runner.run('! ' + atom_name)[0][0]
        # A hack to make runner::&self work
        # TODO? the problem is that we need to return an operation to make this
        # work in parent expressions, thus, it is unclear how to return pure
        # symbols
        if atom.get_metatype() == hp.AtomKind.GROUNDED:
            return atom
        # TODO: borrow atom type to op
        return OperationAtom(
            token,
            lambda *args: run_resolved_symbol_op(runner, atom, *args),
            unwrap=False)

    return {
        r"[^\s]+::[^\s]+": lambda token: resolve_atom(metta, token)
    }


class REPL:
    def __init__(self, runner):
        self.history = []  # Initialize command history
        self.runner = runner

    def main_loop(self):
        while True:
            try:
                line = input("metta> ")  # Use input function for user input

                if line == '.history':
                    for idx, item in enumerate(self.history):
                        log_message(USER, f"{idx + 1}: {item}")
                    continue

                # If input is not empty, evaluate it
                if line:
                    self.history.append(line)
                    result = self.runner.run(line)
                    if result is not None:
                        log_message(USER, result)

            except (KeyboardInterrupt, EOFError):
                # Exit REPL gracefully when Ctrl-D (EOF) is pressed
                log_message(USER, "\nExiting REPL...")
                return

            except Exception as e:
                log_message(USER, f"Error: {e}")
                if METTALOG_VERBOSE >= DEBUG:
                    traceback.print_exc()

# Main function
def main():
    global runner, runnerAtom
    import hyperon
    if runner is None:
    cb = hyperon.Environment.custom_env(working_dir=os.path.dirname("."))
        runner = MeTTaVS(env_builder=cb)
    if runnerAtom is None:
        atom = hyperon.atoms.AtomType.ATOM
        runnerAtom = G(runner, atom)

    # Process command-line arguments and track if REPL or files were handled
    wont_need_repl = process_args(runner)

    # If no files were loaded and --repl wasn't triggered, enter REPL
    if not wont_need_repl:
        REPL(runner).main_loop()

if __name__ == "__main__":
    main()

