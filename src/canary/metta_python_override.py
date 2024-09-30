# metta_python_override.py
import inspect
import janus_swi
import traceback
import types
from enum import Enum
from platform import python_branch
from functools import wraps  # Import wraps here
from typing import Any, Dict, List, Tuple, Union

# ANSI color codes
RED = '\033[31m'
GREEN = '\033[32m'
YELLOW = '\033[33m'
BLUE = '\033[34m'
MAGENTA = '\033[35m'
CYAN = '\033[36m'
RESET = '\033[0m'
WHITE = '\033[0m'

import inspect
import types
import builtins




def fill_value_from_method(method):
    """
    Extracts parameter names and formats them for Prolog use in the form of 'X,Y,...'.
    Handles different types of parameters (positional, keyword-only, defaults).
    Now includes support for pybind11-wrapped functions.
    """

    def is_pybind11_function(obj) -> bool:
        """Detect if the object is a pybind11-wrapped function."""
        return isinstance(obj, types.BuiltinFunctionType) and hasattr(obj, '__doc__')

    def get_pybind11_signature(obj, unknown=Any) -> str:
        """Extract the signature from the docstring of a pybind11-wrapped function."""
        doc = obj.__doc__

        # Safe environment with built-ins to avoid unsafe eval
        safe_globals = {name: getattr(builtins, name) for name in dir(builtins) if not name.startswith('__')}

        def resolve_type(param_type: str) -> Union[str, Any]:
            try:
                return eval(param_type, {"__builtins__": None}, safe_globals)
            except Exception:
                return param_type  # Fallback to string representation

        if doc:
            first_line = doc.splitlines()[0]
            if '(' in first_line and ')' in first_line:
                param_str = first_line[first_line.find('(') + 1:first_line.find(')')]
                param_list = [param.strip() for param in param_str.split(',') if param.strip()]

                param_names = []
                for param in param_list:
                    if ':' in param:
                        name, param_type = param.split(':', 1)
                        resolved_type = resolve_type(param_type.strip())
                        param_names.append(name.strip().capitalize())
                    else:
                        param_names.append(param.strip().capitalize())

                return ",".join(param_names)  # Join param names in "X,Y" format

        return "Unknown,Value"
    try:
        # Check if the method is pybind11-based
        if is_pybind11_function(method):
            value = get_pybind11_signature(method)
            return value

        # For regular Python functions, get the signature using inspect
        sig = inspect.signature(method)
        param_names = []

        for param_name, param in sig.parameters.items():
            # Handle positional-only, keyword-only, and regular arguments
            if param.kind in (inspect.Parameter.POSITIONAL_ONLY, inspect.Parameter.POSITIONAL_OR_KEYWORD):
                param_names.append(param_name.capitalize())
            elif param.kind == inspect.Parameter.KEYWORD_ONLY:
                # For keyword-only arguments, you might append it in a special way if needed
                param_names.append(param_name.capitalize())

            # Handle default values (if you want to represent them)
            if param.default != inspect.Parameter.empty:
                param_names[-1] += f"={param.default}"

        # Join parameter names in the format 'X,Y,...'
        value = ",".join(param_names) if param_names else "None"  # Handle methods with no parameters
        return value

    except Exception as e:
        # Handle cases where method signature retrieval fails
        print(f"Error in extracting parameters from method {method}: {e}")
        return "unknown"


def generate_prolog_rule(full_function_name, cls, method, retval="RetVal"):
    """Dynamically generate the Prolog rule based on the method signature."""
    value = fill_value_from_method(method)
    cls = type_name(cls).capitalize()
    value = value.replace("Self", cls)
    value = value.replace("Cls", cls)
    retval = type_name(retval).capitalize()
    pred_invoke = f"{full_function_name}({value},{retval})"
    return f"{pred_invoke}:- "


def type_name(cls):
    try:
        cls = cls.__name__
    except:
        cls = str(cls)
    return cls


def name_dot(cls, name):
    if cls is None:
        return type_name(name)
    if name is None:
        return type_name(cls)
    return f"{type_name(cls)}.{type_name(name)}"
    
# Helper functions used by the Overrider
def uses_self(member):
    if not callable(member):
        return False
    try:
        sig = inspect.signature(member)
        return 'self' in sig.parameters
    except (ValueError, TypeError):
        return False

def implemented_in(member_name, obj):
    if inspect.isclass(obj):
        if member_name in obj.__dict__:
            return obj.__name__
        for base in inspect.getmro(obj)[1:]:
            if member_name in base.__dict__:
                return base.__name__
    elif inspect.ismodule(obj):
        if member_name in obj.__dict__:
            return obj.__name__
        else:
            return 'imported'
    return None

class MemberLevel(Enum):
    INSTANCE = "instance"  # For instance methods and properties
    CLASS = "class"        # For class methods, static methods, and class variables
    MODULE = "module"      # For module-level members

class Implemented(Enum):
    LOCAL = "Local"          # Implemented in the current class/module
    INHERITED = "Inherited"  # Inherited from a base class
    DEFAULT = "Default"      # Inherited from 'object'

def classify_implementation(obj, implemented_from):
    if implemented_from == obj.__name__:
        return Implemented.LOCAL.value
    elif implemented_from == 'object':
        return Implemented.DEFAULT.value
    elif implemented_from is not None:
        return Implemented.INHERITED.value
    else:
        return Implemented.LOCAL.value

# Create a module type for Janus and register necessary functions
janus_module = types.ModuleType("janus")
janus_module.import_module_from_string = janus_swi.import_module_from_string
modules_by_name = {}

def call_python_original(full_function_name, args, largs, kwargs):
    print(f"{GREEN}call_python_original with full function name={repr(full_function_name)}, args={repr(args)}, largs={repr(largs)}, kwargs={repr(kwargs)}{RESET}")
    module_name, function_name = full_function_name.split("_", 1)
    module = modules_by_name.get(module_name)
    if not module:
        raise ValueError(f"Module {module_name} not found.")
    if hasattr(module, '_original_functions'):
        overridden_functions = getattr(module, '_original_functions')
        func = overridden_functions.get(function_name)
    else:
        func = getattr(module, function_name, None)
    if func is None:
        raise AttributeError(f"Function {function_name} not found in module {module_name}.")
    return func(*args, **kwargs)

import builtins
def signature(obj, unknown=Any) -> Tuple[List[Dict[str, Union[str, Any]]], Union[str, Any]]:
    """
    Retrieve the signature of a Python function or a pybind11-wrapped function.

    Parameters:
        obj: The object (function or method) whose signature is to be retrieved.
        unknown: The value to use when the type or parameter details are unknown. Defaults to Any.

    Returns:
        A tuple: (list of parameters, return type)
        Each parameter is represented as a dictionary with keys: 'name', 'type', and 'default'.
    """
    def is_pybind11_function(obj) -> bool:
        """Detect if the object is a pybind11-wrapped function."""
        return isinstance(obj, types.BuiltinFunctionType) and hasattr(obj, '__doc__')

    def get_pybind11_signature(obj) -> Tuple[List[Dict[str, Union[str, Any]]], Union[str, Any]]:
        doc = obj.__doc__
        nonlocal unknown
        # Define a safe environment with built-in types and objects
        safe_globals = {name: getattr(builtins, name) for name in dir(builtins) if not name.startswith('__')}
        def resolve_type(param_type):
            if str(param_type) == "":
                return unknown
            # Temporarily disable this function
            if True:
                return param_type
            try:
                return eval(param_type, {"__builtins__": None}, safe_globals)
            except (NameError, SyntaxError, TypeError):
                return param_type  # Fallback to the string representation if resolution fails
        if doc:
            first_line = doc.splitlines()[0]
            if '(' in first_line and ')' in first_line:
                param_str = first_line[first_line.find('(') + 1:first_line.find(')')]
                param_list = [param.strip() for param in param_str.split(',') if param.strip()]
                parsed_params = []
                for param in param_list:
                    if ':' in param:
                        name, param_type = param.split(':', 1)
                        param_type = param_type.strip()
                        resolved_type = resolve_type(param_type)
                        parsed_params.append({'name': name.strip(), 'type': resolved_type, 'default': None})
                    else:
                        parsed_params.append({'name': param.strip(), 'type': unknown, 'default': None})
                return_type = unknown
                if '->' in first_line:
                    return_type_str = first_line.split('->')[-1].strip()
                    return_type = resolve_type(return_type_str)
                return parsed_params, return_type
        # Fallback if the docstring is missing or in an unexpected format
        return [{'name': 'unknown_param', 'type': unknown, 'default': None}], unknown

    # Try to retrieve the standard function signature using inspect
    try:
        sig = inspect.signature(obj)
        parameters = []
        for param_name, param in sig.parameters.items():
            param_type = param.annotation if param.annotation is not inspect.Parameter.empty else unknown
            param_default = param.default if param.default is not inspect.Parameter.empty else None
            parameters.append({'name': param_name, 'type': param_type, 'default': param_default})
        return_type = sig.return_annotation if sig.return_annotation is not inspect.Signature.empty else unknown
        return parameters, return_type
    except (ValueError, TypeError) as e:
        if is_pybind11_function(obj):
            return get_pybind11_signature(obj)
        print(f"{RED}inspect.signature({name_dot(getattr(obj, '__class__', None), getattr(obj, '__name__', str(obj)))}) caused error: {e}{RESET}")
        return [{'name': 'builtin_method', 'type': unknown, 'default': None}], unknown
    except Exception as e:
        print(f"{RED}Error determining signature for {name_dot(getattr(obj, '__class__', None), getattr(obj, '__name__', str(obj)))}: {e}{RESET}")
        return [{'name': 'unknown_param', 'type': unknown, 'default': None}], unknown

def predicate_exists_in_prolog(predicate_name):
    """
    Check if a given predicate exists in the Prolog environment.
    """
    query = f"current_predicate({predicate_name}/_)"
    result = list(janus_swi.query(query))
    return bool(len(result) > 0)

def override_module_calls_with_janus(module, prefix=None):
    if not hasattr(module, '_original_functions'):
        module._original_functions = {}
    original_functions = module._original_functions
    module_name = module.__name__
    modules_by_name[module_name] = module
    for name, member in inspect.getmembers(module):
        if inspect.isroutine(member):
            #print(f"{BLUE}Processing function {name_dot(module, name)}{RESET}")
            override_function(module, name, member, original_functions, prefix)
        elif inspect.isclass(member):
            print(f"{BLUE}Processing class {name_dot(module, name)}{RESET}")
            override_static_fields_with_janus(member, prefix)
            override_static_calls_with_janus(member, prefix)
            override_instance_fields_with_janus(member, prefix)
            override_instance_calls_with_janus(member, prefix)

def override_function(module, name, func, original_functions, prefix):
    if prefix is None:
        module_name = module.__name__
        prefix = module_name
    full_function_name = f"{prefix}_{name}"
    if full_function_name in original_functions:
        return
    original_functions[full_function_name] = func
    
    # Generate Prolog rule
    prolog_rule = generate_prolog_rule(full_function_name, module, func)
    
    # Check if predicate exists in Prolog
    if predicate_exists_in_prolog(full_function_name):
        print(f"{WHITE} {prolog_rule} {GREEN} Overriding {name_dot(module, name)}{RESET}")
        register_method(full_function_name, func, module, name)
    else:
        print(f"{WHITE} {prolog_rule} {YELLOW} Not Overriding {name_dot(module, name)}{RESET}")

    def make_overridden_function(original_function, full_function_name):
        @wraps(original_function)
        def overridden_function(*args, **kwargs):
            pargs = list(args)
            try:
                # Pass module to Prolog
                result, tf = try_one(full_function_name, args, pargs, kwargs, original_function=original_function)
                if tf:
                    return result
            except Exception as e:
                traceback.print_exc()
                print(f"{RED}First attempt failed for {full_function_name}, retrying... Exception: {e}{RESET}")
                try:
                    py_break()
                    result, tf = try_one(full_function_name, args, pargs, kwargs, original_function=original_function)
                    if tf:
                        return result
                except Exception as retry_exception:
                    print(f"{RED}Second attempt failed for {full_function_name}. Exception: {retry_exception}{RESET}")

            # If try_one fails twice, fall back to the original function
            return original_function(*args, **kwargs)
        return overridden_function

    overridden_func = make_overridden_function(func, full_function_name)
    setattr(module, name, overridden_func)


def register_method(full_function_name, func, module, name):
    #print(f"{GREEN}Overriding {name_dot(module, name)} {WHITE} {full_function_name}.{RESET}")
    param_details, return_type = signature(func)
    param_names = [param['name'] for param in param_details]
    param_types = [str(param['type']) for param in param_details]
    param_defaults = [param['default'] for param in param_details]
    try:
        janus_swi.cmd('user', 'register_function', full_function_name, param_names, param_types, param_defaults,
                      str(return_type))
    except Exception as e:
        print(f"{RED}Error registering function {name_dot(module, name)} with Prolog: {e}{RESET}")


def override_static_calls_with_janus(cls, prefix=None):
    if not hasattr(cls, '_original_methods'):
        cls._original_methods = {}
    original_methods = cls._original_methods
    class_name = cls.__name__
    if prefix is None:
        prefix = class_name
    for method_name, method in inspect.getmembers(cls, predicate=inspect.isfunction):
        if uses_self(method):
            continue
        full_function_name = f"static_{prefix}_{method_name}"
        if full_function_name in original_methods:
            continue
        
        # Generate Prolog rule
        prolog_rule = generate_prolog_rule(full_function_name, cls, method)
        
        # Check if predicate exists in Prolog
        if predicate_exists_in_prolog(full_function_name):
            print(f"{WHITE} {prolog_rule} {GREEN} Overriding {name_dot(cls, method_name)}{RESET}")
            register_method(full_function_name, method, cls, method_name)
            original_methods[full_function_name] = method
        else:
            print(f"{WHITE} {prolog_rule} {YELLOW} Not Overriding {name_dot(cls, method_name)}{RESET}")

        def make_overridden_method(original_method, full_function_name):
            @wraps(original_method)
            def overridden_method(*args, **kwargs):
                pargs = list(args)
                try:
                    # Pass class to Prolog
                    result, tf = try_one(full_function_name, args, [cls] + pargs, kwargs, original_function=original_method)
                    if tf:
                        return result
                except Exception as e:
                    print(f"{RED}First attempt failed for {full_function_name}, retrying... Exception: {e}{RESET}")
                    try:
                        py_break()
                        result, tf = try_one(full_function_name, args, [cls] + pargs, kwargs, original_function=original_method)
                        if tf:
                            return result
                    except Exception as retry_exception:
                        print(f"{RED}Second attempt failed for {full_function_name}. Exception: {retry_exception}{RESET}")
                return original_method(*args, **kwargs)
            return overridden_method

        overridden_method = make_overridden_method(method, full_function_name)
        setattr(cls, method_name, overridden_method)

def override_static_fields_with_janus(cls, prefix=None):
    """
    Overrides static fields of a class with Prolog-backed attributes, passing class to Prolog.
    """
    class_name = cls.__name__
    if prefix is None:
        prefix = class_name
    for field_name, value in cls.__dict__.items():
        if not field_name.startswith('_') and not callable(value):
            if uses_self(value): continue
            full_field_name = f"static_{prefix}_{field_name}"
            
            # Generate Prolog rule
            prolog_rule = generate_prolog_rule(full_field_name, cls, value)
            
            # Check if predicate exists in Prolog
            if predicate_exists_in_prolog(full_field_name):
                print(f"{WHITE} {prolog_rule} {GREEN} Overriding {name_dot(cls, field_name)}{RESET}")
                def make_prolog_property(ffn):
                    def getter(clsp):
                        result = janus_swi.apply_once('user', f"get_{ffn}", clsp)
                        if result:
                            return result
                        else:
                            raise AttributeError(f"Prolog field {ffn} not found")
                    def setter(clsp, new_value):
                        janus_swi.cmd('user',f"set_{ffn}",clsp,new_value)
                    return property(getter, setter)
                setattr(cls, field_name, make_prolog_property(full_field_name))
            else:
                print(f"{WHITE} {prolog_rule} {YELLOW} Not Overriding {name_dot(cls, field_name)}{RESET}")

def override_instance_fields_with_janus(cls, prefix=None):
    """
    Overrides instance fields of a class with Prolog-backed attributes, passing `self` to Prolog.
    This includes handling properties and fields from __slots__.
    """
    original_init = cls.__init__
    if prefix is None:
        prefix = cls.__name__

    @wraps(original_init)
    def new_init(self, *args, **kwargs):
        original_init(self, *args, **kwargs)
        # Collect all attribute names from the instance and class
        attr_names = set()

        # Collect instance attribute names, if __dict__ is present
        instance_dict = getattr(self, '__dict__', {})
        attr_names.update(instance_dict.keys())

        # Include attributes from __slots__, if defined
        if hasattr(cls, '__slots__'):
            for slot in cls.__slots__:
                attr_names.add(slot)

        # Include properties and other descriptors from class
        for name, value in inspect.getmembers(cls):
            if isinstance(value, property):
                attr_names.add(name)

        # Now override instance fields
        for field_name in attr_names:
            full_field_name = f"instance_{prefix}_{field_name}"

            # Generate Prolog rule
            prolog_rule = generate_prolog_rule(full_field_name, cls, field_name)

            # Check if predicate exists in Prolog
            if predicate_exists_in_prolog(full_field_name):
                print(f"{WHITE} {prolog_rule} {GREEN} Overriding {name_dot(cls, field_name)}{RESET}")
                
                # Replace the field with a Prolog-backed property
                def make_prolog_property(full_field_name=full_field_name):
                    def getter(self):
                        # Pass self to Prolog to get the value
                        result = janus_swi.apply_once("user",f"{full_field_name}",self)
                        if result:
                            return result
                        else:
                            raise AttributeError(f"Prolog field {full_field_name} not found")
                    def setter(self, new_value):
                        # Pass self to Prolog to set the value
                        janus_swi.cmd("user", f"set_{full_field_name}", self, new_value)
                    
                    return property(getter, setter)
                setattr(cls, field_name, make_prolog_property())
            else:
                print(f"{WHITE} {prolog_rule} {YELLOW} Not Overriding {name_dot(cls, field_name)}{RESET}")

    cls.__init__ = new_init

def override_instance_calls_with_janus(cls, prefix=None):
    """
    Overrides instance methods of a class with a given prefix and ensures `self` is passed to Prolog.
    """
    if not hasattr(cls, '_original_methods'):
        cls._original_methods = {}
    original_methods = cls._original_methods
    class_name = cls.__name__
    if prefix is None:
        prefix = class_name
    for method_name, method in inspect.getmembers(cls, predicate=inspect.isfunction):
        implemented_from = implemented_in(method_name, cls)
        if not uses_self(method): 
            continue
        full_function_name = f"instance_{prefix}_{method_name}"
        if full_function_name in original_methods:
            continue

        # Generate Prolog rule
        prolog_rule = generate_prolog_rule(full_function_name, cls, method)
        
        # Check if predicate exists in Prolog
        if predicate_exists_in_prolog(full_function_name):
            print(f"{WHITE} {prolog_rule} {GREEN} Overriding {name_dot(cls, method_name)}{RESET}")
            register_method(full_function_name, method, cls, method_name)
            original_methods[full_function_name] = method
        else:
            print(f"{WHITE} {prolog_rule} {YELLOW} Not Overriding {name_dot(cls, method_name)}{RESET}")
            continue

        def make_overridden_instance_method(original_method, full_function_name):
            @wraps(original_method)
            def overridden_instance_method(self, *args, **kwargs):
                try:
                    result, tf = try_one(full_function_name, (self,) + args, [self] + list(args), kwargs, original_function=original_method)
                    if tf:
                        return result
                except Exception as e:
                    print(f"{RED}First attempt failed for {full_function_name}, retrying... Exception: {e}{RESET}")
                    try:
                        py_break()
                        result, tf = try_one(full_function_name, [self] + list(args), [self] + list(args), kwargs, original_function=original_method)
                        if tf:
                            return result
                    except Exception as retry_exception:
                        print(f"{RED}Second attempt failed for {full_function_name}. Exception: {retry_exception}{RESET}")
                return original_method(self, *args, **kwargs)
            return overridden_instance_method

        overridden_instance_method = make_overridden_instance_method(method, full_function_name)
        setattr(cls, method_name, overridden_instance_method)

rust_mode=False
def try_one(full_function_name, args, pargs, kwargs, original_function):
    global rust_mode
    if rust_mode:
            return original_function(*args, **kwargs), True
    result = janus_swi.apply_once('user', 'from_python', full_function_name, args, pargs, kwargs)
    if str(result) == 'call_original_function':
        try:
            rust_mode = True
            return original_function(*args, **kwargs), True
        finally:
            rust_mode = False

    return result, True

def py_break():
    pass

# Example usage to test module and class overrides
def test_my_module():
    setup_janus()
    import types

    # Create an actual module object
    my_module = types.ModuleType("my_module")

    # Define functions in this module
    def add(a, b):
        return 666  # Intentionally incorrect
    def multiply(a, b):
        return a * b
    def greet(name):
        return f"Hello, {name}!"
    def factorial(n):
        if n == 0:
            return 1
        else:
            return -n  # Intentionally incorrect

    # Assign these functions to the module
    my_module.add = add
    my_module.multiply = multiply
    my_module.greet = greet
    my_module.factorial = factorial

    # Create a class to test class method and field overrides
    class MyClass:
        class_field = "Original class field"
        __slots__ = ['slot_field', 'instance_field']
        def __init__(self):
            self.instance_field = "Instance Field Value"  # Now this is allowed
            self.slot_field = "Slot Field Value"
    
        @property
        def prop_field(self):
            return "Property Field Value"
    
        @prop_field.setter
        def prop_field(self, value):
            pass  # Setter logic
        @staticmethod
        def static_method_example(x):
            return x * 2
        def say_hello(self):
            return "Hello from MyClass!"
        def multiply(self, x, y):
            return x * y            
    
    # Now override the instance fields
    override_instance_fields_with_janus(MyClass)
    
    # Create an instance and access the fields
    obj = MyClass()
    print(obj.instance_field)  # Should interact with Prolog if the predicate exists
    print(obj.slot_field)      # Should interact with Prolog if the predicate exists
    print(obj.prop_field)      # Should interact with Prolog if the predicate exists

    # Assign the class to the module
    my_module.MyClass = MyClass
    # Override the module calls with Janus
    print(f"{CYAN}Overriding module calls with Janus:{RESET}")
    override_module_calls_with_janus(my_module)
    # Test function calls
    print(f"{CYAN}\nTesting overridden functions:{RESET}")
    print(f"add(2, 3): {my_module.add(2, 3)}")
    print(f"multiply(2, 3): {my_module.multiply(2, 3)}")
    print(f"greet('Alice'): {my_module.greet('Alice')}")
    print(f"factorial(5): {my_module.factorial(5)}")
    # Test class method and field calls
    obj = my_module.MyClass()
    print(f"{CYAN}\nTesting overridden class methods:{RESET}")
    print(f"obj.say_hello(): {obj.say_hello()}")
    print(f"obj.multiply(4, 5): {obj.multiply(4, 5)}")
    # Test static method
    print(f"{CYAN}\nTesting overridden static method:{RESET}")
    print(f"MyClass.static_method_example(10): {my_module.MyClass.static_method_example(10)}")
    print(f"{CYAN}\nTesting overridden class fields:{RESET}")
    print(f"MyClass.class_field: {my_module.MyClass.class_field}")
    print(f"obj.instance_field: {obj.instance_field}")
    print(f"obj.slot_field: {obj.slot_field}")
    print(f"obj.prop_field: {obj.prop_field}")
    print(f"obj.multiply(4, 5): {obj.multiply(4, 5)}")
    # Test static method
    print(f"{CYAN}\nTesting overridden class fields:{RESET}")
    print(f"MyClass.class_field: {my_module.MyClass.class_field}")
    print(f"obj.instance_field: {obj.instance_field}")

# Helper functions to setup Janus and Hyperon overrides
janus_ready = False
def setup_janus():
    global janus_ready
    if janus_ready:
        return True
    janus_ready = True
    import sys
    import os
    sys.path.append(os.path.dirname(os.path.abspath(__file__)))

    # Initialize Janus
    prolog_module_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'metta_corelib.pl')
    consult_command = f"consult('{prolog_module_path}')"
    janus_swi.query_once(consult_command)


OverrideHyperonPy=False
hyperon_overrides_ready = False
def load_hyperon_overrides():
    global hyperon_overrides_ready
    if hyperon_overrides_ready:
        return True
    hyperon_overrides_ready = True
    import hyperon
    override_static_calls_with_janus(hyperon.atoms.Atom, "hyperon_atom")
    override_static_fields_with_janus(hyperon.atoms.Atom, "hyperon_atom")
    #override_instance_fields_with_janus(hyperon.atoms.Atom, "hyperon_atom")
    #override_instance_calls_with_janus(hyperon.atoms.Atom, "hyperon_atom")
    if OverrideHyperonPy:
        override_hyperon_py()

def override_hyperon_py():
    import hyperonpy
    override_module_calls_with_janus(hyperonpy)
    # Include the missing line as per your request
    override_static_fields_with_janus(hyperonpy.CAtomType, "catom_type_")

def override_metta_py():
    import hyperon
    override_instance_calls_with_janus(hyperon.runner.MeTTa)
    override_static_calls_with_janus(hyperon.runner.MeTTa)

def test_hyperon_overrides():
    setup_janus()
    load_hyperon_overrides()
    override_metta_py()
    import hyperon
    metta = hyperon.runner.MeTTa()
    result = metta.run("!(+ 1 1)")
    print(result)

if __name__ == "__main__":
    setup_janus()
    test_my_module()
    test_hyperon_overrides()
