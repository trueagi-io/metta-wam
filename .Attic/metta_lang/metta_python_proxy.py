#!/usr/bin/env python3

import inspect
import os
from enum import Enum
import sys
import types
import hyperon
import atexit
import traceback

# Module-level verbosity levels
SILENT = 0  # No output
USER = 1    # Basic output for users
DEBUG = 2   # Detailed debug information
TRACE = 3   # Granular trace-level output
DEVEL = DEBUG
# Default verbosity level
METTALOG_VERBOSE = USER
global runner
runner = None
global norepl_mode
norepl_mode=True
global rust_mode
rust_mode=True
global catom_mode
catom_mode=False
global cb
cb = None


# Log messages based on verbosity level
def mesg(level, message='', m2=None, m3=None):
    if not isinstance(level, int):
        message = f"{level} {message}"
        level = TRACE

    message = f"{message} {m2 or ''} {m3 or ''}"
    message = message.replace('lib/python3.11/site-packages/hyperonpy.cpython-311-x86_64-linux-gnu.so','..')
    message = message.replace("on <module 'hyperonpy' from '/home/deb12user/metta-wam/venv/..'>",'..')

    print(message)
    return
    if METTALOG_VERBOSE >= level:
        print(message)

def set_no_mock_objects_flag(flag: bool):
    global mock_throw_error
    mock_throw_error = flag

def get_no_mock_objects_flag() -> bool:
    return mock_throw_error
# Testing and Examples


# Set verbosity level
def set_verbosity(level):
    global METTALOG_VERBOSE
    level = int(level)
    if level in [SILENT, USER, DEBUG, TRACE]:
        METTALOG_VERBOSE = level
        mesg(DEBUG, f"Verbosity set to level {level}")
    else:        
        print(f"Invalid verbosity level '{level}' provided. Defaulting to level: {METTALOG_VERBOSE}.")
        METTALOG_VERBOSE = USER

# Initialize verbosity from environment variable
try:
    set_verbosity(os.getenv("METTALOG_VERBOSE", USER))
except Exception as e:
    mesg(USER, f"An error occurred: {e}")
    if METTALOG_VERBOSE >= DEBUG:
        traceback.print_exc()

# Command-line verbosity handling
try:
    i = 0
    while i < len(sys.argv):
        arg = sys.argv[i]
        i += 1

        if arg in ("-v", "--verbosity"):
            try:
                set_verbosity(sys.argv[i])
                break
            except (ValueError, IndexError):
                print("Invalid verbosity level. Defaulting to USER.")
                set_verbosity(USER)

    mesg(DEBUG, f"Argv={sys.argv}")

except Exception as e:
    mesg(USER, f"An error occurred: {e}")
    if METTALOG_VERBOSE >= DEBUG:
        traceback.print_exc()

# History file for REPL
def install_history():
    histfile = os.path.join(os.path.expanduser("~"), ".metta_history")
    import readline
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


# Print help information
def print_help():
    # Get the script name dynamically
    script_name = os.path.basename(sys.argv[0])  # This will get the current script's name

    help_text = f"""
MeTTaLog Runner/REPL in Python

Usage: {script_name} [options] [files]

Options:
  -v, --verbosity <level>   Set verbosity level: 0 (SILENT), 1 (USER), 2 (DEBUG), 3 (TRACE)
  -p, --path <path>         Add search paths for modules
  -f, --file <file>         MeTTa script files to execute
  --repl                    Enter REPL
  --version                 Print the version and exit
  --help                    Display this help message
    """
    print(help_text)



def make_runner():
    global runner
    if runner is not None:
        return runner
    #mesg(DEBUG, f"hyperonpy::make_runner")
    global cb
    if cb is None: cb = hyperon.Environment.custom_env(working_dir=os.path.dirname("."))
    return hyperon.runner.MeTTa(env_builder=cb)

# Load and execute a file
def load_file(file):    
    global runner

    norepl_mode = True
    if file == "--repl":
        # Start REPL immediately and ignore further arguments
        REPL().main_loop()
        return

    try:
        mesg(DEBUG, f"Executing MeTTa script from: {file}")
        with open(file) as f:
            program = f.read()        
            if runner is None: runner = make_runner()
            for result in runner.run(program):
                mesg(USER, f"Result: {result}")
    except FileNotFoundError:
        mesg(USER, f"Error: File '{file}' not found.")
    except Exception as e:
        mesg(USER, f"An error occurred: {e}")
        if METTALOG_VERBOSE >= DEBUG:
            traceback.print_exc()

import hyperonpy as hp
from hyperon.atoms import Atom, AtomType, OperationAtom
from hyperon.base import GroundingSpaceRef, Tokenizer, SExprParser
from hyperonpy import EnvBuilder, ModuleId

# Global variables
suspend_trace = False
rust_mode = False  # Set to False as default

def with_suspend_trace(action, *args, **kwargs):
    """
    Temporarily suspends trace by setting suspend_trace to True, 
    performs the action, then restores the original trace state.
    """
    global suspend_trace
    was = suspend_trace
    suspend_trace = True    
    try:
        # Execute the action
        result = action(*args, **kwargs)
    finally:
        # Restore the previous trace state
        suspend_trace = was
    
    return result

def into_repr(r):
    """
    Recursively converts the input into its repr equivalent, while handling special types like lists, tuples, 
    dicts, sets, and hp.CAtom (with trace suspension).
    """

    if isinstance(r, (str, int, float, bool)): return r

    # Check if r is a property and avoid treating it as iterable
    if isinstance(r, property):
        try:
            # Attempt to get the property value using str() as an alternative to repr()
            return str(r.fget()) if r.fget else '<unreadable property>'
        except Exception as e:
            return f'<unreadable property: {e}>'

    if isinstance(r, str):
        if r.startswith('"') and r.endswith('"'):
            return r
        if r.startswith("'") and r.endswith("'"):
            return r
        return repr(r)
    
    elif isinstance(r, dict):
        return {into_repr(k): into_repr(v) for k, v in r.items()}

    elif isinstance(r, set):
        return set(into_repr(i) for i in r)

    if isinstance(r, list):
        return [into_repr(i) for i in r]
    
    elif isinstance(r, tuple):
        return tuple(into_repr(i) for i in r)
    
    elif isinstance(r, hp.CAtom):        
        # Convert CAtom to string with trace suspension
        return with_suspend_trace(hp.atom_to_str, r)
        #return hp.atom_to_str(r)
    
    else:
        # For other types, return their string representation
        #return repr(r)
        return repr(r)
    return repr(r)
    
def processed_directive(line):
    """Process REPL directives by converting dot-prefixed commands to hyphenated arguments."""
    
    global norepl_mode, rust_mode, catom_mode

    # Check if the line matches the pattern for setting a global variable
    if '=' in line:
        # Split the line into variable and value
        var_name, value = line.split('=', 1)
        var_name = var_name.strip()
        value = value.strip()

        # Map the string input to actual boolean or other values
        if value.lower() in ('true', '1'):
            value = True
        elif value.lower() in ('false', '0'):
            value = False
        else:
            try:
                # Try to convert the value to an int or float if possible
                value = eval(value)
            except:
                # Otherwise, leave it as a string
                pass

        # Handle setting global variables
        if var_name == 'norepl_mode':
            norepl_mode = value
            mesg(USER, f"norepl_mode set to {norepl_mode}")
        elif var_name == 'rust_mode':
            rust_mode = value
            mesg(USER, f"rust_mode set to {rust_mode}")
        elif var_name == 'catom_mode':  # Updated from catom_mode to catom_mode
            catom_mode = value
            mesg(USER, f"catom_mode set to {catom_mode}")
        else:
            mesg(USER, f"Unknown setting: {var_name}")
        return True

    # Convert the first dot into '--' to standardize dot commands into hyphen commands
    if line.startswith('.'):
        if len(line) == 2:  # Shortcuts like .p should map to -p
            line = '-' + line[1:]  # Convert `.p` to `-p`
        else:
            line = '--' + line[1:]  # Convert full commands like `.path` to `--path`

    # Split the input line into arguments
    args = line.split()

    # Handle the commands (now in hyphenated format)
    if args[0].startswith('-'):
        arg = args[0]

        if arg == '-history' or arg == '--history':
            # Assuming `repl` is an instance of the REPL class or accessible here
            for idx, item in enumerate(repl.history):
                mesg(USER, f"{idx + 1}: {item}")
            return True

        elif arg == '--version':
            mesg(USER, f"Hyperon version: {hyperon.__version__}")
            return True

        elif len(args) > 1 and arg in ("--file", '-f'):
            filename = args[1]
            load_file(filename)
            mesg(USER, f"File {filename} loaded.")
            return True

        elif arg in ("--verbosity", '-v'):
            if len(args) > 1:
                try:
                    verbosity_level = int(args[1])
                    set_verbosity(verbosity_level)
                except ValueError:
                    print("Invalid verbosity level. Defaulting to USER.")
                    set_verbosity(USER)
            return True

        # Handle the `--path` or `-p` directive with the provided path
        elif arg in ("--path", '-p'):
            if len(args) > 1:  # Ensure that the path is provided
                path = args[1]
                mesg(DEBUG, f"Adding path: {path}")
                global cb
                if cb is None: cb = hyperon.Environment.custom_env(working_dir=os.path.dirname("."))
                hp.env_builder_push_include_path(cb, path)
            else:
                mesg(USER, "Error: No path provided for -p or --path")
            return True

        elif arg in ("--help", '-h'):
            print_help()
            return True

    # If no known directive or argument is matched, return False
    return False

# Manual command-line argument processing in real-time
def process_args():
    global norepl_mode
    norepl_mode = False  # Keep track if we won't need to start the REPL
    i = 1  # Skip the first argument (script name)

    while i < len(sys.argv):
        arg = sys.argv[i]

        # Combine the cases where the argument expects a following value (like a file or verbosity level)
        if arg in ("--verbosity", "-v", "--file", "-f", "--path", "-p") and i + 1 < len(sys.argv):
            directive = f"{arg} {sys.argv[i + 1]}"
            i += 1  # Skip the next item (since it's part of the directive)
        else:
            directive = arg

        # Call `processed_directive` with the combined directive
        if processed_directive(directive):
            # If processed_directive returns True, handle any flags that imply we won't need the REPL
            if arg in ("--file", "-f"):
                norepl_mode = True
        else:
            # If the argument is not handled by `processed_directive`, it's assumed to be a file
            load_file(arg)
            norepl_mode = True  # Mark that a file was loaded

        i += 1

    return norepl_mode

# REPL for interactive input with command history support
class REPL:
    def __init__(self, runner=None):
        self.history = []  # Initialize command history
        self.runner = runner

    def main_loop2(self):
        mettalog.repl(self.runner)

    def main_loop(self):
        install_history()
        while True:
            try:
                line = input("metta> ")  # Use input function for user input

                # If input is not empty, evaluate it
                if line:
                    self.history.append(line)
                    if processed_directive(line): continue

                    if self.runner is None:
                        global runner
                        if runner is None: runner = make_runner()
                        self.runner = runner

                    global catom_mode
                    if not catom_mode:
                        presult = self.runner.run(line)
                        print("=>", presult)
                    else:
                        trunner = self.runner
                        parser = SExprParser(line)
                        results = hp.metta_run(trunner.cmetta, parser.cparser)
                        err_str = hp.metta_err_str(trunner.cmetta)
                        if (err_str is not None):
                            print(f"RuntimeError({into_repr(err_str)})")
                            continue

                        tresults = into_repr(results)

                        #if flat:
                        #return [Atom._from_catom(catom) for result in results for catom in result]
                        #else:
                        #return [[Atom._from_catom(catom) for catom in result] for result in results]

                        print("->", tresults)
                    #if result is not None:
                    #    mesg(USER, "{result}")

            except (KeyboardInterrupt, EOFError):
                # Exit REPL gracefully when Ctrl-D (EOF) is pressed
                mesg(USER, "\nExiting REPL...")
                return

            except Exception as e:
                mesg(USER, f"Repl Error: {e}")
                #if METTALOG_VERBOSE >= DEBUG:
                traceback.print_exc()







def name_dot(module, name):
    if module is None:
        return name
    return f"{module.__name__}.{name}"

import inspect

def signature(obj):
    try:
        # First, try Python's inspect.signature for standard functions
        return inspect.signature(obj)
    except (ValueError, TypeError) as e:
        # Handle specific case where signature is not found for built-in methods
        if is_pybind11_function(obj):
            # Custom fallback for pybind11 methods, if known
            return get_pybind11_signature(obj)
        print(f"inspect.signature({obj}) caused error: {e}")
        return "(builtin method)"
    except Exception as e:
        # Catch other errors
        print(f"Error determining signature for {obj}: {e}")
        return "(unknown)"

def is_pybind11_function(obj):
    """Detect if the object is a pybind11-wrapped function."""
    return isinstance(obj, types.BuiltinFunctionType) and hasattr(obj, '__doc__')

def get_pybind11_signature(obj):
    """Retrieve or construct a pybind11 function's signature."""
    # If the function has a docstring with signature info
    doc = obj.__doc__
    if doc:
        # Try to extract the first line which often contains the signature
        first_line = doc.splitlines()[0]
        if '(' in first_line and ')' in first_line:
            return first_line.strip()

    # Return fallback if no signature is found in docstring
    return "(pybind11 function with unknown signature)"


def ignore_exception(*args, **kwargs):
    pass





























import inspect
import types

# Global variables
suspend_trace = False
rust_mode = False  # Assuming this is defined elsewhere
TRACE = True  # Assuming this is a logging level

synth_members = {
    '__module__', '__filename__', '__name__', 'pybind11_type',
    '__class__', 'observer', '__dict__', '__weakref__',
    '__getattribute__', '__setattr__', '__delattr__', '__new__'
}

# 1. Observer Class
class Observer:
    def __init__(self):
        self.observers = {}

    def subscribe(self, event_type, callback, level="instance"):
        """Subscribe to an event type ('before_call', 'after_call', 'set', 'get') with a callback."""
        key = f"{level}_{event_type}"
        if key not in self.observers:
            self.observers[key] = []
        self.observers[key].append(callback)

    def notify(self, func, event_type, *args, **kwargs):
        """Notify all subscribers for a specific event type."""
        global suspend_trace
        if suspend_trace:
            return

        level = kwargs.pop('level', 'instance')  # Remove 'level' from kwargs
        args_list = list(args)

        # Skip certain substrings in the name (adjust or remove based on your needs)
        if len(args_list) > 1:
            for substring in ["tokenizer", "metta_err_str", "_free"]:
                if substring in str(args_list[1]):
                    return

        if isinstance(args_list[0], types.ModuleType):
            args_list[0] = None

        args = tuple(args_list)

        # Generate the event key based on the level and event type
        event_key = f"{level}_{event_type}"

        # Check if there are any observers subscribed to this event key
        if event_key in self.observers:
            for callback in self.observers[event_key]:
                result = callback(*args)
                if result is not None:
                    return result

        # Default return if no observer modifies the value
        return kwargs.get('default_value')

# 2. Helper Functions for Instance Getters/Setters
def create_instance_getter(private_name, target, observer, level, value):
    """A non-self function to handle the instance getter logic."""
    def instance_getter(self):
        try:
            current_value = getattr(self, private_name)
            if rust_mode:
                return current_value

            if 'observer' not in self.__dict__:
                self.__dict__['observer'] = observer

            result = observer.notify(value, 'get', target, private_name, current_value, level=level)

            if isinstance(result, dict) and result.get('just_return', False):
                return result.get('just_return')
            return result if result is not None else current_value
        except Exception as e:
            mesg(TRACE, f"Error getting instance property '{private_name}' in {target.__name__}: {e}")
            raise
    return instance_getter

def create_instance_setter(private_name, target, observer, level, value):
    """A non-self function to handle the instance setter logic."""
    def instance_setter(self, new_value):
        try:
            if rust_mode:
                return setattr(self, private_name, new_value)

            if 'observer' not in self.__dict__:
                self.__dict__['observer'] = observer

            result = observer.notify(value, 'set', target, private_name, new_value, level=level)

            if isinstance(result, dict) and result.get('do_not_really_set', False):
                return
            setattr(self, private_name, result if result is not None else new_value)
        except Exception as e:
            mesg(TRACE, f"Error setting instance property '{private_name}' in {target.__name__}: {e}")
            raise
    return instance_setter

# 3. Monkey Patching Class
class MonkeyPatcher:
    def __init__(self, observer):
        self.observer = observer
        self.patched_objects = {}
        self.patched_modules = {}
        self.patched_instance_classes = {}
        self.patched_static_classes = {"str": True}  # Dictionary to store patched classes

    def patch_instance_property(self, target, property_name, value, level, is_instance=False):
        """Patch instance-level properties to observe changes (on_set) and accesses (on_get)."""
        private_name = f"_{property_name}"
        try:
            setattr(target, private_name, value)
            mesg(TRACE, f"[Trace] Patching instance property: {property_name} in {target.__name__}")

            instance_getter = create_instance_getter(private_name, target, self.observer, level, value)
            instance_setter = create_instance_setter(private_name, target, self.observer, level, value)

            setattr(target, property_name, property(fget=instance_getter, fset=instance_setter))
        except Exception as e:
            mesg(TRACE, f"Failed to patch instance property '{property_name}' in {target.__name__}: {e}")
            raise

    def patch_static_property(self, cls, property_name, value):
        """Patch class (static) properties to allow observation of gets and sets with exception handling."""
        private_name = f"_{property_name}"
        try:
            setattr(cls, private_name, value)
            mesg(TRACE, f"[Trace] Patching static property: {property_name} in {cls.__name__}")

            def class_getter():
                try:
                    current_value = getattr(cls, private_name)
                    if rust_mode:
                        return current_value
                    result = self.observer.notify(value, 'get', cls, property_name, current_value, level="static")
                    if isinstance(result, dict) and result.get('just_return', False):
                        return result.get('just_return')
                    return result if result is not None else current_value
                except Exception as e:
                    mesg(TRACE, f"Error getting static property '{property_name}' in {cls.__name__}: {e}")
                    raise

            def class_setter(new_value):
                try:
                    if rust_mode:
                        return setattr(cls, private_name, new_value)
                    result = self.observer.notify(value, 'set', cls, property_name, new_value, level="static")
                    if isinstance(result, dict) and result.get('do_not_really_set', False):
                        return
                    setattr(cls, private_name, result if result is not None else new_value)
                except Exception as e:
                    mesg(TRACE, f"Error setting static property '{property_name}' in {cls.__name__}: {e}")
                    raise

            setattr(cls, property_name, property(fget=class_getter, fset=class_setter))
        except Exception as e:
            mesg(TRACE, f"Failed to patch static property '{property_name}' in {cls.__name__}: {e}")
            raise

    def patch_module_property(self, module, property_name, value):
        """Patch module properties to allow observation of gets and sets with exception handling."""
        private_name = f"_{property_name}"
        try:
            setattr(module, private_name, value)
            mesg(TRACE, f"[Trace] Patching module property: {property_name} in {module.__name__}")

            def module_getter():
                try:
                    current_value = getattr(module, private_name)
                    if rust_mode:
                        return current_value
                    result = self.observer.notify(value, 'get', module, property_name, current_value, level='module')
                    if isinstance(result, dict) and result.get('do_not_really_get', False):
                        return result.get('return_value')
                    return current_value
                except Exception as e:
                    mesg(TRACE, f"Error getting module property '{name_dot(module, property_name)}': {e}")
                    raise

            def module_setter(new_value):
                try:
                    if rust_mode:
                        return setattr(module, private_name, new_value)
                    result = self.observer.notify(value, 'set', module, property_name, new_value, level='module')
                    if isinstance(result, dict) and result.get('do_not_really_set', False):
                        return
                    setattr(module, private_name, result if result is not None else new_value)
                except Exception as e:
                    mesg(TRACE, f"Error setting module property '{property_name}' in {module.__name__}: {e}")
                    raise

            setattr(module, property_name, property(fget=module_getter, fset=module_setter))
        except Exception as e:
            mesg(TRACE, f"Failed to patch module property '{property_name}' in {module.__name__}: {e}")
            raise

    def patch_static_method(self, target, method_name, method, level="static"):
        """Patch a static method to observe calls with before/after hooks and error handling."""
        original_method = method
        mesg(TRACE, f"[Trace] Patching static method: {method_name} in {target.__name__}")

        def patched_static_method(*args, **kwargs):
            result = self.observer.notify(original_method, 'before_call', target, method_name, args, kwargs, level=level)
            if isinstance(result, dict) and result.get('do_not_really_call', False):
                return result.get('just_return')
            modified_args, modified_kwargs = result if isinstance(result, tuple) else (args, kwargs)

            try:
                result = original_method(*modified_args, **modified_kwargs)
            except Exception as e:
                mesg(f"Error in static method {method_name}: {e}")
                raise

            modified_result = self.observer.notify(original_method, 'after_call', target, method_name, result, level=level)
            return modified_result if modified_result is not None else result

        setattr(target, method_name, patched_static_method)

    def patch_instance_method(self, target, method_name, method, level):
        """Patch an instance method to observe calls with before/after hooks and error handling."""
        original_method = method
        mesg(TRACE, f"[Trace] Patching instance method: {method_name} in {target.__name__}")

        def patched_instance_method(instance_self, *args, **kwargs):
            result = self.observer.notify(original_method, 'before_call', target, method_name, args, kwargs, level=level)
            if isinstance(result, dict) and result.get('do_not_really_call', False):
                return result.get('just_return')
            modified_args, modified_kwargs = result if isinstance(result, tuple) else (args, kwargs)

            try:
                result = original_method(instance_self, *modified_args, **modified_kwargs)
            except Exception as e:
                mesg(f"Error in instance method {method_name}: {e}")
                raise

            modified_result = self.observer.notify(original_method, 'after_call', target, method_name, result, level=level)
            return modified_result if modified_result is not None else result

        setattr(target, method_name, patched_instance_method)

    def patch_module_function(self, module, func_name, func):
        """Patch a function or callable in the module with argument/result modifications and error handling."""
        original_function = func
        mesg(TRACE, f"[Trace] Patching module function: {func_name} in {module.__name__}")

        def patched_function(*args, **kwargs):
            result = self.observer.notify(original_function, 'before_call', module, func_name, args, kwargs, level='module')
            if isinstance(result, dict) and result.get('do_not_really_call', False):
                return result.get('just_return')
            modified_args, modified_kwargs = result if isinstance(result, tuple) else (args, kwargs)

            try:
                result = original_function(*modified_args, **modified_kwargs)
            except Exception as e:
                mesg(f"Error in function {func_name}: {e}")
                raise

            modified_result = self.observer.notify(original_function, 'after_call', module, func_name, result, level='module')
            return modified_result if modified_result is not None else result

        setattr(module, func_name, patched_function)

    def patch_module(self, module):
        """Patch all classes, functions, and properties in a given module (module level)."""
        name_key = module.__name__

        if name_key in self.patched_modules:
            mesg(TRACE, f"Module {name_key} already patched.")
            return

        self.patched_modules[name_key] = module
        mesg(TRACE, f"Patching module: {name_key}")

        for name, obj in inspect.getmembers(module):
            if name in synth_members:
                continue

            try:
                if inspect.isclass(obj):
                    mesg(TRACE, f"Skipping class {name}")
                    # Optionally, you can patch classes here
                elif inspect.isfunction(obj) or inspect.isbuiltin(obj) or callable(obj):
                    mesg(TRACE, f"Patching function or callable: {name}")
                    self.patch_module_function(module, name, obj)
                else:
                    mesg(TRACE, f"Attempting to patch non-callable property: {name}")
                    self.patch_module_property(module, name, obj)
            except Exception as e:
                mesg(TRACE, f"Skipping patch for {name} in module {module.__name__}: {e}")

    def _is_patchable_type(self, obj):
        """Check if the object's type is patchable."""
        unpatchable_types = (str, float, list, tuple, dict, set, int, bool, Observer)
        return not isinstance(obj, unpatchable_types)

    def patch_class(self, cls, instance=None):
        """Patch both static and instance members of a class."""
        if not self._is_patchable_type(cls):
            return

        self.patch_static_class(cls)
        self.patch_instance_class(cls, instance)

    def patch_static_class(self, cls):
        """Patch static methods and properties of a class."""
        name_key = cls.__name__

        if name_key in self.patched_static_classes:
            mesg(TRACE, f"Static class {name_key} already patched.")
            return

        self.patched_static_classes[name_key] = cls
        mesg(TRACE, f"Patching static class: {name_key}")

        try:
            instance = cls()
        except Exception as e:
            mesg(TRACE, f"Unable to create instance of {name_key}: {e}")
            instance = None

        for name, attribute in inspect.getmembers(cls):
            if name in synth_members:
                continue

            try:
                if callable(attribute):
                    if inspect.isfunction(attribute) and not hasattr(attribute, '__self__'):
                        mesg(TRACE, f"Patching static or class method: {name} in {cls.__name__}")
                        self.patch_static_method(cls, name, attribute, level="static")
                    else:
                        mesg(TRACE, f"Skipping instance method: {name} in {cls.__name__}")
                else:
                    if hasattr(cls, name) and (instance is None or not hasattr(instance, name)):
                        mesg(TRACE, f"Patching class-level or static property: {name} in {cls.__name__}")
                        self.patch_static_property(cls, name, attribute)
                    else:
                        mesg(TRACE, f"Skipping instance property: {name} in {cls.__name__}")
            except Exception as e:
                mesg(TRACE, f"Skipping patch for {name} in {cls.__name__}: {e}")

    def patch_instance_class(self, cls, instance=None):
        """Patch instance methods and properties of a class."""
        name_key = cls.__name__

        if name_key in self.patched_instance_classes:
            mesg(TRACE, f"Instance class {name_key} already patched.")
            return

        self.patched_instance_classes[name_key] = cls
        mesg(TRACE, f"Patching instance class: {name_key}")

        if instance is None:
            try:
                instance = cls()
            except Exception as e:
                mesg(TRACE, f"Unable to create instance of {name_key}: {e}")
                instance = None

        for name, attribute in inspect.getmembers(cls):
            if name in synth_members:
                continue

            try:
                if callable(attribute):
                    if inspect.isfunction(attribute) and hasattr(attribute, '__code__') and 'self' in attribute.__code__.co_varnames:
                        mesg(TRACE, f"Patching instance method: {name} in {cls.__name__}")
                        self.patch_instance_method(cls, name, attribute, level="instance")
                    else:
                        mesg(TRACE, f"Skipping static method: {name} in {cls.__name__}")
                else:
                    if instance is not None and hasattr(instance, name):
                        mesg(TRACE, f"Patching instance property: {name} in {cls.__name__}")
                        self.patch_instance_property(cls, name, attribute, level="instance", is_instance=True)
                    else:
                        mesg(TRACE, f"Skipping static property: {name} in {cls.__name__}")
            except Exception as e:
                mesg(TRACE, f"Skipping patch for {name} in {cls.__name__}: {e}")

    def patch_object(self, obj):
        """Patch an individual instance (instance-level tracking)."""
        if not self._is_patchable_type(obj):
            return

        name_key = id(obj)

        if name_key in self.patched_objects:
            return

        self.patched_objects[name_key] = obj
        mesg(TRACE, f"Patching object: {name_key}")

        if not hasattr(obj, 'observer'):
            setattr(obj, 'observer', self.observer)

        cls = obj.__class__
        self.patch_class(cls, obj)

# 3. Example usage
class MyClass:
    class_value = 100  # Static property
    other_class_value = 666  # Another static property

    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return repr(value)

    def my_method(self, x):
        return x * 2

    @staticmethod
    def static_method(y):
        return y + 100

    @classmethod
    def class_method(cls, z):
        return z * 3

class AnotherClass:
    def __init__(self, name):
        self.name = name

    def greet(self):
        return f"Hello, {self.name}"

# Instantiate Observer and MonkeyPatcher
observer = Observer()
patcher = MonkeyPatcher(observer)

# Example usage for patching modules:
# Patch the module

# invoked with:   py_call(load_metta_python_proxy:patch_hyperonpy(),O)
def patch_hyperonpy():    
    from hyperon.atoms import SymbolAtom, ExpressionAtom, GroundedAtom, VariableAtom, ValueAtom
    from hyperon.runner import MeTTa
    import hyperonpy  # Assuming hyperonpy is an external module you want to patch
    VA = ValueObject("string")
    patcher.patch_instance_class(ValueObject,instance=VA)
    patcher.patch_class(MyClass)
    line = input("ValueObject> ")
    patcher.patch_module(hyperonpy)
    line = input("hyperonpy> ")
    metta = MeTTa()
    patcher.patch_instance_class(MeTTa,instance=metta)
    line = input("MeTTa> ")
    
    return {}

has_applied_monkey_patches = None

def apply_monkey_patches():
    global has_applied_monkey_patches
    
    # Check if monkey patches have already been applied
    if has_applied_monkey_patches is not None and has_applied_monkey_patches is True:
        return

    mesg(DEBUG, f"hyperonpy::apply_monkey_patches")
    # Set the flag to indicate that the patches are being applied
    has_applied_monkey_patches = True

    # Importing inside the function to avoid circular import issues
    from hyperon.atoms import SymbolAtom, ExpressionAtom, GroundedAtom, VariableAtom, ValueAtom

    def monkey_patch_static_class(cls):
        # Save the original __init__ method
        original_init = cls.__init__
        
        # Define the new __init__ method
        def new_init(self, *args, **kwargs):
            original_init(self, *args, **kwargs)
            # Only add 'value' if it doesn't already exist
            if not hasattr(self, 'value'):
                self.value = self
        
        # Only patch __init__ to add 'value' if missing
        cls.__init__ = new_init

        # Only add get_object if it does not already exist
        if not hasattr(cls, 'get_object'):
            cls.get_object = lambda self: self

        # Only add __iter__ if it does not already exist
        if not hasattr(cls, '__iter__'):
            cls.__iter__ = lambda self: iter([self])

        # Only add __repr__ if it does not already exist
        if not hasattr(cls, '__repr__'):
            cls.__repr__ = lambda self: str_repr(self)

        # Only add __str__ if it does not already exist
        if not hasattr(cls, '__str__'):
            cls.__str__ = lambda self: str_repr(self)

        # Check if the class already has a __getitem__ method
        original_getitem = getattr(cls, '__getitem__', None)

        # Define the new __getitem__ method
        def new_getitem(self, index):
            # If there was an original __getitem__, call it
            if original_getitem:
                try:
                    return original_getitem(self, index)
                except Exception as e:
                    print(f"Original __getitem__ raised an exception: {e}")
            # Otherwise, return the instance itself
            return self

        # Monkey patch the __getitem__ method
        cls.__getitem__ = new_getitem

    # Apply the monkey patch function to multiple classes
    monkey_patch_static_class(SymbolAtom)
    monkey_patch_static_class(ExpressionAtom)
    monkey_patch_static_class(VariableAtom)
    monkey_patch_static_class(GroundedAtom)
    monkey_patch_static_class(ValueAtom)
    monkey_patch_static_class(hyperonpy.CAtom)

def str_repr(obj):
   return "{type(obj)}@{id(obj)}" 

def args_and_kwargs(args, kwargs):
    if len(kwargs)==0:
        return f"with args={into_repr(args)}"
    return f"with args={into_repr(args)}, kwargs={into_repr(kwargs)}"

# Define the instance_before_call and after_call hooks
def instance_before_call(obj, method_name, args, kwargs):
    """Hook called before the method is invoked."""
    mesg(DEVEL,f"[Before] I {method_name} on {obj} {args_and_kwargs(args, kwargs)}")
    # Modify the arguments before the method call
    args = tuple(arg * 2 if isinstance(arg, (int, float)) else arg for arg in args)
    # Optionally return a result without calling the method
    if method_name == 'my_method' and args[0] == 10:
        return {'do_not_really_call': True, 'just_return': 999}
    return args, kwargs

def instance_after_call(obj, method_name, result):
    """Hook called after the method is invoked."""
    if result is None: return
    r = into_repr(result)
    mesg(DEVEL,f"[After] I {method_name} on {obj} -> {r}")
    # Modify the result after the method call
    return result + 10 if isinstance(result, (int, float)) else result

def static_before_call(obj, method_name, args, kwargs):
    """Hook called before a static/class method is invoked."""
    mesg(DEVEL,f"[Before] Static/class method {method_name} on {obj} {args_and_kwargs(args, kwargs)}")
    # Modify the arguments before the method call
    args = tuple(arg + 5 if isinstance(arg, (int, float)) else arg for arg in args)
    return args, kwargs

def static_after_call(obj, method_name, result):
    """Hook called after a static/class method is invoked."""
    if result is None: return
    mesg(DEVEL,f"[After] Static/class method {method_name} on {obj} -> {result}")
    # Modify the result after the method call
    return result * 2 if isinstance(result, (int, float)) else result

def instance_on_get(obj, property_name, value):
    """Hook called when getting a property."""
    mesg(DEVEL,f"[Instance] Property {property_name} accessed on {obj} with value {value}")
    # Fake the value of the property
    if property_name == 'value':
        return {'just_return': 1000}
    patcher.patch_object(value)
    return value

def instance_on_set(obj, property_name, new_value):
    """Hook called when setting a property."""
    mesg(DEVEL,f"[Instance] Property {property_name} changed on {obj} to {new_value}")
    # Skip setting the value for the 'value' property
    if property_name == 'value':
        return {'do_not_really_set': True}
    return new_value

def static_on_get(obj, property_name, value):
    """Hook called when getting a static property."""
    mesg(DEVEL,f"[Static] Property {property_name} accessed on {obj} with value {value}")
    # Fake the value of the static property
    if property_name == 'class_value':
        return {'just_return': 9999}
    return value

def static_on_set(obj, property_name, new_value):
    """Hook called when setting a static property."""
    mesg(DEVEL,f"[Static] Property {property_name} changed on {obj} to {new_value}")
    # Skip setting the value for the static property 'class_value'
    if property_name == 'class_value':
        return {'do_not_really_set': True}
    return new_value

# Subscribe to instance-level events (before and after call hooks, get and set hooks)
observer.subscribe('before_call', instance_before_call, level="instance")
observer.subscribe('after_call', instance_after_call, level="instance")
observer.subscribe('get', instance_on_get, level="instance")
observer.subscribe('set', instance_on_set, level="instance")

# Subscribe to static-level events (before and after call hooks, get and set hooks)
observer.subscribe('before_call', static_before_call, level="static")
observer.subscribe('after_call', static_after_call, level="static")
observer.subscribe('get', static_on_get, level="static")
observer.subscribe('set', static_on_set, level="static")

#apply_monkey_patches()

# Main function
def demo1():
    global norepl_mode
    global patcher
    global observer
    # Example: Patch a class and an instance
    my_obj = MyClass(10)
    patcher.patch_object(my_obj)  # This will patch both the class (static) and the instance (instance-level)

    # Example: Patch another class and an instance
    another_obj = AnotherClass("Alice")
    patcher.patch_object(another_obj)

    # Call methods and access properties (before/after call hooks, get/set hooks will be triggered)
    print(my_obj.my_method(5))  # Will trigger before/after call hooks
    mesg(USER,my_obj.my_method(10))  # Will trigger before_call and return just_return without calling the method
    mesg(USER,my_obj.value)  # Will trigger get hook and return a faked value
    my_obj.value = 20  # Will trigger set hook but skip setting the actual value

    # Accessing static properties
    mesg(USER,MyClass.class_value)  # Will trigger static get hook and return a faked value
    MyClass.class_value = 200  # Will trigger static set hook but skip setting the actual value

    mesg(USER,MyClass.static_method(50))  # Will trigger before/after call hooks
    mesg(USER,MyClass.class_method(30))  # Will trigger before/after call hooks

    mesg(USER,another_obj.greet())  # Will trigger both instance and static 'call' event, return modified result

def main():
    global norepl_mode
    global patcher
    global observer

    
    # If no files were loaded and --repl wasn't triggered, enter REPL
    norepl_mode = False
    # Process command-line arguments and track if REPL or files were handled
    norepl_mode = process_args()
        
    #patcher.patch(cb)
    #if cb is None cb = hyperon.Environment.custom_env(working_dir=os.path.dirname("."))
    #runner = hyperon.MeTTa(env_builder=cb)
    # patcher.patch(runner)
    # patcher.patch(hyperon)
    # Process command-line arguments and track if REPL or files were handled


    #mesg(DEBUG,"hyperonpy::main")
    #apply_monkey_patches()
    
    # If no files were loaded and --repl wasn't triggered, enter REPL
    if not norepl_mode:
        load_file("--repl")


if __name__ == "__main__":
    main()


