import inspect
import sys
import os
import sys
import types
import hyperon
import readline
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

# Log messages based on verbosity level
def mesg(level, message='', m2=None, m3=None):
    if not isinstance(level, int):
        message = f"{level} {message}"
        level = TRACE

    message = f"{message} {m2 or ''} {m3 or ''}"
    message = message.replace('lib/python3.11/site-packages/hyperonpy.cpython-311-x86_64-linux-gnu.so','..')
    message = message.replace("on <module 'hyperonpy' from '/home/deb12user/metta-wam/venv/..'>",'..')

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
        print(f"Invalid verbosity level '{level}' provided. Defaulting to USER level.")
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

global runner
runner = None
global wont_need_repl
wont_need_repl=True

# Manual command-line argument processing in real-time
def process_args():
    global wont_need_repl
    wont_need_repl = False  # Keep track if we won't need to start the REPL
    i = 1  # Skip the first argument (script name)

    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg in ("--demo"):
            # demo0()
            demo1()
            #demo2()

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
                mesg(DEBUG, f"Adding path: {path}")
                env_builder_push_include_path(cb,path)
                i += 1
        elif arg == "--version":
            mesg(USER, f"Hyperon version: {hyperon.__version__}")
            sys.exit(0)  # Exit after showing version

        elif arg in ("-h", "--help"):        
            print_help()
            sys.exit(0)  # Exit after showing help

        elif arg in ("-f", "--file"):
            if i + 1 < len(sys.argv):
                file = sys.argv[i + 1]                
                load_file(file)
                wont_need_repl = True  # Mark that a file was loaded
                i += 1

        else:
            # Assume this is a file to process
            load_file(arg)
            wont_need_repl = True  # Mark that a file was loaded

        i += 1

    return wont_need_repl

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
    mesg(DEBUG, f"hyperonpy::make_runner")
    return hyperon.runner.MeTTa(env_builder=hyperon.Environment.custom_env(working_dir=os.path.dirname(".")))

# Load and execute a file
def load_file(file):    
    global runner

    wont_need_repl = True
    if file == "--repl":
        # Start REPL immediately and ignore further arguments
        if runner is None: 
            runner = make_runner()
        REPL(runner).main_loop()
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

suspend_trace = False

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
        #return with_suspend_trace(hp.atom_to_pyobj, r)
        return atom_to_str(r)
    
    else:
        # For other types, return their string representation
        #return repr(r)
        return repr(r)
    return repr(r)

    


# REPL for interactive input with command history support
class REPL:
    def __init__(self, runner):
        self.history = []  # Initialize command history
        self.runner = runner

    def main_loop(self):
        install_history()
        while True:
            try:
                line = input("metta> ")  # Use input function for user input

                if line == '.history':
                    for idx, item in enumerate(self.history):
                        mesg(USER, f"{idx + 1}: {item}")
                    continue

                # If input is not empty, evaluate it
                if line:
                    self.history.append(line)
                    if self.runner is None:
                        global runner
                        if runner is None: runner = make_runner()
                        self.runner = runner
        #            result = self.runner.run(line)
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



synth_members = {'__class__', '__dict__', '__weakref__', '__getattribute__', '__setattr__', '__delattr__', '__new__'}
# 1. Observer Class
class Observer:
    global suspend_trace

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
        if suspend_trace: return
        level = kwargs.pop('level', 'instance')  # Remove 'level' from kwargs
        #print(f"n={func} ",end="", flush=True)
        #kwargs['original'] = func
        #args = ((args,func), kwargs)
        args_list = list(args)
        for substring in ["tokenizer", "metta_err_str", "_free"]:
            if substring in args_list[1]: return
        if isinstance(args_list[0],(types.ModuleType)): 
            args_list[0]=None
        arg = tuple(args)

#        print()
        instance_key = f"instance_{event_type}"
        static_key = f"static_{event_type}"

        # Handle instance-level events
        if level == "instance" and instance_key in self.observers:
            for callback in self.observers[instance_key]:
                return callback(*args)

        # Handle static-level events
        if level == "static" and static_key in self.observers:
            for callback in self.observers[static_key]:
                return callback(*args)

        # Default return if no observer modifies the value
        return kwargs.get('default_value')

import inspect
import sys

# 2. Monkey Patching Class
class MonkeyPatcher:
    
    def __init__(self, observer):
        self.observer = observer
        self.patched_classes = {"str": True}  # Dictionary to store patched classes

    def patch(self, obj):
        """Patch an object by getting its class and calling patch_class."""
        cls = obj.__class__
        self.patch_class(cls)
        self._patch_instance(obj)

    def patch_class(self, cls):
        """Monkey-patch all methods and properties of a class (static level)."""
        class_name = cls.__name__
        
        # Check if class has already been patched
        if class_name in self.patched_classes:
            mesg(f"Class {class_name} already patched.")
            return

        mesg(f"Patching class: {class_name}")
        
        for name, attribute in inspect.getmembers(cls):
            if name in synth_members: continue
            if name.startswith('__') and name.endswith('__'):
                continue
            try:
                if inspect.isfunction(attribute) or inspect.ismethod(attribute):
                    self._patch_method(cls, name, attribute, level="static")
                else:
                    # Handle static properties
                    self._patch_class_property(cls, name, attribute)
            except Exception as e:
                mesg(f"Skipping patch_class {name}: {e}")

        # Store the patched class in the dictionary
        self.patched_classes[class_name] = cls


    def patch_module(self, module):
        """Patch all classes and functions in a given module (static level)."""
        for name, obj in inspect.getmembers(module):
            if inspect.isclass(obj):
                # Skip built-in data types and already-patched classes
                if obj.__name__ not in self.patched_classes and not self._is_builtin_type(obj):
                    self.patch_class(obj)
            elif inspect.isfunction(obj) or inspect.isbuiltin(obj) or callable(obj):
                # Patch both Python and C-implemented functions
                mesg(f"Patching function or callable: {name}")
                self._patch_function(module, name, obj)


    def patch_module2(self, module):
        """Patch all classes and functions in a given module (static level)."""
        for name, obj in inspect.getmembers(module):
            if inspect.isclass(obj):
                # Skip built-in data types and already-patched classes
                if obj.__name__ not in self.patched_classes and not self._is_builtin_type(obj):
                    self.patch_class(obj)
            elif inspect.isfunction(obj):
                # Optionally, patch module-level functions
                self._patch_function(module, name, obj)


    def _patch_function(self, module, func_name, func):
        """Patch module-level functions or C-level callables."""
        original_function = func

        def patched_function(*args, **kwargs):
            # Notify before the function call
            result = self.observer.notify(original_function,'before_call', module, func_name, args, kwargs)
            if isinstance(result, dict) and result.get('do_not_really_call', False):
                return result.get('just_return')

            # Call the original function (whether it's Python or C)
            try:
                result = original_function(*args, **kwargs)
            except Exception as e:
                mesg(f"E in func_name {func_name}: {e}")

            # Notify after the function call
            modified_result = self.observer.notify(func,'after_call', module, func_name, result)
            return modified_result if modified_result is not None else result

        # Replace the function in the module
        setattr(module, func_name, patched_function)


    def _is_builtin_type(self, cls):
        """Check if the class is a built-in data type."""
        return isinstance(cls, (str, int, float, bool, list, dict, set, tuple))

    def _patch_function2(self, module, func_name, func):
        """Patch module-level functions."""
        original_function = func

        def patched_function(*args, **kwargs):
            # Notify before the function call
            result = self.observer.notify(func,'before_call', module, func_name, args, kwargs)
            if isinstance(result, dict) and result.get('do_not_really_call', False):
                return result.get('just_return')

            # Call the original function
            result = original_function(*args, **kwargs)
            
            # Notify after the function call
            modified_result = self.observer.notify(func,'after_call', module, func_name, result)
            return modified_result if modified_result is not None else result

        # Replace the function in the module
        setattr(module, func_name, patched_function)

    def _patch_class_property(self, cls, property_name, value):
        """Patch class (static) properties to allow observation of gets and sets."""
        private_name = f"_{property_name}"

        # Make the property private by storing it in the class dictionary
        setattr(cls, private_name, value)

        # Define a custom getter for the class attribute
        def class_getter(self):
            current_value = getattr(self, private_name)
            result = patcher.observer.notify(value,'get', cls, property_name, current_value, level="static")
            if isinstance(result, dict) and result.get('just_return', False):
                return result.get('just_return')
            return result if result is not None else current_value

        # Define a custom setter for the class attribute
        def class_setter(self, new_value):
            result = patcher.observer.notify(value,'set', cls, property_name, new_value, level="static")
            if isinstance(result, dict) and result.get('do_not_really_set', False):
                return
            setattr(cls, private_name, result if result is not None else new_value)

        # Create the property with the custom getter and setter
        setattr(cls, property_name, property(fget=class_getter, fset=class_setter))

    def _patch_instance(self, obj):
        """Patch an individual instance (instance-level tracking)."""
        # Don't patch objects that are built-in data types
        if isinstance(obj, (str, float, list, tuple, dict, set, int, bool)):
            mesg(f"Skipping patching built-in data type: {type(obj).__name__}")
            return        

        # Ensure the observer is set on the instance
        if not hasattr(obj, 'observer'):
            setattr(obj, 'observer', self.observer)

        for name, attribute in inspect.getmembers(obj):
            if name in synth_members: continue
            if name.startswith('__') and name.endswith('__'):
                continue
            try:
                if inspect.ismethod(attribute):
                    self._patch_method(obj, name, attribute, level="instance")
                else:
                    self._patch_property(obj, name, attribute, level="instance", is_instance=True)
            except Exception as e:
                mesg(f"Skipping _patch_instance {name}: {e}")

    def _patch_method(self, target, method_name, method, level):
        """Patch a method to observe calls with before/after hooks and optionally override the return value."""
        original_method = method

        def patched_method(*args, **kwargs):
            # Call before_call hook
            result = self.observer.notify(method,'before_call', target, method_name, args, kwargs)
            # If before_call returns a dict with do_not_really_call=True, use just_return
            if isinstance(result, dict) and result.get('do_not_really_call', False):
                return result.get('just_return')
            else:
                modified_args, modified_kwargs = result if isinstance(result, tuple) else (args, kwargs)
            # Call the method and get the result
            result = original_method(*modified_args, **modified_kwargs)
            # Call after_call hook, allowing it to modify the result
            modified_result = self.observer.notify(method,'after_call', target, method_name, result)
            return modified_result if modified_result is not None else result

        setattr(target, method_name, patched_method)

    def _patch_property(self, target, property_name, value, level, is_instance=False):
        """Patch a property to observe changes (on_set) and accesses (on_get), allowing modification."""
        private_name = f"_{property_name}"

        # Make the property private
        setattr(target, private_name, value)

        def getter(self):
            current_value = getattr(self, private_name)

            # Access __dict__ directly to avoid triggering the patched getter
            if 'observer' not in self.__dict__:
                self.__dict__['observer'] = patcher.observer

            # Notify the observer, allowing it to modify or fake the value
            result = self.observer.notify(value,'get', target, property_name, current_value, level=level)

            # Check if the observer wants to override the value
            if isinstance(result, dict) and result.get('just_return', False):
                return result.get('just_return')
            return result if result is not None else current_value

        def setter(self, new_value):
            # Access __dict__ directly to avoid triggering the patched setter
            if 'observer' not in self.__dict__:
                self.__dict__['observer'] = patcher.observer

            # Notify the observer, allowing it to modify the value being set or skip setting
            result = self.observer.notify(value,'set', target, property_name, new_value, level=level)

            # If do_not_really_set is True, skip the actual setting
            if isinstance(result, dict) and result.get('do_not_really_set', False):
                return
            setattr(self, private_name, result if result is not None else new_value)




# 3. Example usage
class MyClass:
    class_value = 100  # Static property
    other_class_value = 666  # Another static property

    def __init__(self, value):
        self.value = value

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
import hyperonpy  # Assuming hyperonpy is an external module you want to patch
# Patch the module
patcher.patch_module(hyperonpy)

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

    def monkey_patch_class(cls):
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
    monkey_patch_class(SymbolAtom)
    monkey_patch_class(ExpressionAtom)
    monkey_patch_class(VariableAtom)
    monkey_patch_class(GroundedAtom)
    monkey_patch_class(ValueAtom)
    monkey_patch_class(hyperonpy.CAtom)

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
    patcher.patch(value)
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

apply_monkey_patches()

# Main function
def demo1():
    global wont_need_repl
    global patcher
    global observer
    # Example: Patch a class and an instance
    my_obj = MyClass(10)
    patcher.patch(my_obj)  # This will patch both the class (static) and the instance (instance-level)

    # Example: Patch another class and an instance
    another_obj = AnotherClass("Alice")
    patcher.patch(another_obj)

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
    global wont_need_repl
    global patcher
    global observer

    cb = hyperon.Environment.custom_env(working_dir=os.path.dirname("."))
    #patcher.patch(cb)
    runner = hyperon.MeTTa(env_builder=cb)
    patcher.patch(runner)
    # patcher.patch(hyperon)
    # Process command-line arguments and track if REPL or files were handled

    # If no files were loaded and --repl wasn't triggered, enter REPL
    if not wont_need_repl:
        REPL(runner).main_loop()

    mesg(DEBUG,"hyperonpy::main")
    #apply_monkey_patches()
    
    wont_need_repl = False
    # Process command-line arguments and track if REPL or files were handled
    wont_need_repl = process_args()

    # If no files were loaded and --repl wasn't triggered, enter REPL
    if not wont_need_repl:
        load_file("--repl")

if __name__ == "__main__":
    main()


