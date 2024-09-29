# metta_python_override.py
import inspect
import janus_swi
import traceback
import types
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

def name_dot(cls, name):
    if cls is None:
        return name
    return f"{cls.__name__}.{name}"
    
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
    if not predicate_exists_in_prolog(full_function_name):
        print(f"{YELLOW}Not overriding function {name_dot(module, name)} {WHITE} {full_function_name}.{RESET}")
        return
    print(f"{GREEN}Overriding function {name_dot(module, name)} {WHITE} {full_function_name}.{RESET}")
    param_details, return_type = signature(func)
    param_names = [param['name'] for param in param_details]
    param_types = [str(param['type']) for param in param_details]
    param_defaults = [param['default'] for param in param_details]
    try:
        janus_swi.cmd('user', 'register_function', full_function_name, param_names, param_types, param_defaults, str(return_type))
    except Exception as e:
        print(f"{RED}Error registering function {name_dot(module, name)} with Prolog: {e}{RESET}")

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

def override_static_calls_with_janus(cls, prefix):
    if not hasattr(cls, '_original_methods'):
        cls._original_methods = {}
    original_methods = cls._original_methods
    class_name = cls.__name__
    if prefix is None:
        prefix = class_name
    for method_name, method in inspect.getmembers(cls, predicate=inspect.isfunction):
        full_function_name = f"static_{prefix}_{method_name}"
        if full_function_name in original_methods:
            continue
        if not predicate_exists_in_prolog(full_function_name):
            print(f"{YELLOW}Not overriding static method {name_dot(cls, method_name)} {WHITE} {full_function_name}.{RESET}")
            continue
        print(f"{GREEN}Overriding static method {name_dot(cls, method_name)} {WHITE} {full_function_name}.{RESET}")
        original_methods[full_function_name] = method

        def make_overridden_method(original_method, full_function_name):
            @wraps(original_method)
            def overridden_method(*args, **kwargs):
                pargs = list(args)
                try:
                    # Pass class to Prolog
                    result, tf = try_one(full_function_name, [cls] + pargs, [cls] + pargs, kwargs, original_function=original_method)
                    if tf:
                        return result
                except Exception as e:
                    print(f"{RED}First attempt failed for {full_function_name}, retrying... Exception: {e}{RESET}")
                    try:
                        py_break()
                        result, tf = try_one(full_function_name, [cls] + pargs, [cls] + pargs, kwargs, original_function=original_method)
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
        if not field_name.startswith('__') and not callable(value):
            full_field_name = f"static_{prefix}_{field_name}"
            if predicate_exists_in_prolog(full_field_name):
                print(f"{GREEN}Overriding static field {name_dot(cls, field_name)} {WHITE} {full_field_name}.{RESET}")
                # Replace the field with a property that fetches from Prolog
                def make_prolog_property(ffn):
                    def getter(clsp):
                        # Pass class to Prolog
                        result = janus_swi.apply_once('user',f"set_{ffn}",clsp)
                        if result:
                            return result
                        else:
                            raise AttributeError(f"Prolog field {ffn} not found")
                    def setter(clsp, new_value):
                        # Optional: Implement setter if needed
                        janus_swi.cmd('user',f"set_{ffn}",clsp,new_value)
                    return property(getter, setter)
                mpp = make_prolog_property(full_field_name)
                setattr(cls, field_name, mpp)
            else:
                print(f"{YELLOW}Not overriding static field {name_dot(cls, field_name)} {WHITE} {full_field_name}.{RESET}")
def override_instance_fields_with_janus(cls, prefix=None):
    """
    Overrides instance fields of a class with Prolog-backed attributes, passing self to Prolog.
    """
    original_init = cls.__init__
    if prefix is None:
        prefix = cls.__name__

    @wraps(original_init)
    def new_init(self, *args, **kwargs):
        original_init(self, *args, **kwargs)
        # Collect all attribute names from instance and class
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
            # Include attributes from __slots__, if defined
            elif hasattr(cls, '__slots__'):
                for slot in cls.__slots__:
                    attr_names.add(slot)

        # Now override instance fields
        for field_name in attr_names:
            full_field_name = f"instance_{prefix}_{field_name}"
            if predicate_exists_in_prolog(full_field_name):
                print(f"{GREEN}Overriding instance field {name_dot(cls, field_name)} {WHITE} {full_field_name}.{RESET}")
                # Replace the field with a property that fetches from Prolog
                def make_prolog_property(full_field_name=full_field_name):
                    def getter(self):
                        # Pass self along to Prolog
                        result = janus_swi.apply_once("user",f"{full_field_name}",self)
                        if result:
                            return result
                        else:
                            raise AttributeError(f"Prolog field {full_field_name} not found")
                    def setter(self, new_value):
                        # Optional: Implement setter if needed
                        janus_swi.cmd("user", f"{full_field_name}", self,new_value)
                    return property(getter, setter)
                setattr(cls, field_name, make_prolog_property())
            else:
                print(f"{YELLOW}Not overriding instance field {name_dot(cls, field_name)} {WHITE} {full_field_name}.{RESET}")
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
        full_function_name = f"instance_{prefix}_{method_name}"
        if full_function_name in original_methods:
            continue
        if not predicate_exists_in_prolog(full_function_name):
            print(f"{YELLOW}Not overriding instance method {name_dot(cls, method_name)} {WHITE} {full_function_name}.{RESET}")
            continue
        print(f"{GREEN}Overriding instance method {name_dot(cls, method_name)} {WHITE} {full_function_name}.{RESET}")
        original_methods[full_function_name] = method

        def make_overridden_instance_method(original_method, full_function_name):
            @wraps(original_method)
            def overridden_instance_method(self, *args, **kwargs):
                try:
                    result, tf = try_one(full_function_name, [self] + list(args), [self] + list(args), kwargs, original_function=original_method)
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

def try_one(full_function_name, args, pargs, kwargs, original_function):
    result = janus_swi.apply_once('user', 'from_python', full_function_name, args, pargs, kwargs)
    if str(result) == 'call_original_function':
        return original_function(*args, **kwargs), True
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

hyperon_overrides_ready = False
def load_hyperon_overrides():
    global hyperon_overrides_ready
    if hyperon_overrides_ready:
        return True
    hyperon_overrides_ready = True
    import hyperonpy
    override_module_calls_with_janus(hyperonpy)
    import hyperon
    override_static_calls_with_janus(hyperon.atoms.Atom, "hyperon_atom")
    override_static_fields_with_janus(hyperon.atoms.Atom, "hyperon_atom")
    #override_instance_fields_with_janus(hyperon.atoms.Atom, "hyperon_atom")
    #override_instance_calls_with_janus(hyperon.atoms.Atom, "hyperon_atom")
    # Include the missing line as per your request
    override_static_fields_with_janus(hyperonpy.CAtomType, "catom_type_")

def test_hyperon_overrides():
    setup_janus()
    load_hyperon_overrides()
    import hyperon
    override_instance_calls_with_janus(hyperon.runner.MeTTa)
    metta = hyperon.runner.MeTTa()
    result = metta.run("!(+ 1 1)")
    print(result)

if __name__ == "__main__":
    setup_janus()
    test_my_module()
    test_hyperon_overrides()
