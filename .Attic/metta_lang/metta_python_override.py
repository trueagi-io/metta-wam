
# override.py
import janus_swi as janus
import inspect
import types
import traceback
from functools import wraps  # Import wraps here

modules_by_name = {}

def call_python_original(full_function_name, args, largs, kwargs):
    print(f"call_python_original with full function name={repr(full_function_name)}, args={repr(args)}, largs={repr(largs)}, kwargs={repr(kwargs)}")
    module_name, function_name = full_function_name.split("_", 1)
    module = modules_by_name[module_name]

    if hasattr(module, '_original_functions'):
        overridden_functions = getattr(module, '_original_functions')
        func = overridden_functions.get(function_name)
    else:
        func = None

    if func is None:
        func = getattr(module, function_name)

    return func(*args, **kwargs)


def signature(obj):
    def is_pybind11_function(obj):
        """Detect if the object is a pybind11-wrapped function."""
        return isinstance(obj, types.BuiltinFunctionType) and hasattr(obj, '__doc__')

    def get_pybind11_signature(obj):
        """Retrieve or construct a pybind11 function's signature as a list of parameter details with return type."""
        doc = obj.__doc__
        if doc:
            first_line = doc.splitlines()[0]
            if '(' in first_line and ')' in first_line:
                param_str = first_line[first_line.find('(') + 1:first_line.find(')')]
                param_list = [param.strip() for param in param_str.split(',') if param.strip()]

                parsed_params = []
                for param in param_list:
                    if ':' in param:
                        name, param_type = param.split(':', 1)
                        parsed_params.append({
                            'name': name.strip(),
                            'type': param_type.strip(),
                            'default': None
                        })
                    else:
                        parsed_params.append({
                            'name': param.strip(),
                            'type': 'unknown',
                            'default': None
                        })

                return_type = 'unknown'
                if '->' in first_line:
                    return_type = first_line.split('->')[-1].strip()

                return parsed_params, return_type

        return [{'name': 'unknown_param', 'type': 'unknown', 'default': None}], 'unknown'

    try:
        sig = inspect.signature(obj)
        parameters = []

        for param_name, param in sig.parameters.items():
            param_type = param.annotation if param.annotation is not inspect.Parameter.empty else 'Any'
            param_default = param.default if param.default is not inspect.Parameter.empty else None

            parameters.append({
                'name': param_name,
                'type': param_type,
                'default': param_default
            })

        return_type = sig.return_annotation if sig.return_annotation is not inspect.Signature.empty else 'Any'

        return parameters, return_type
    except (ValueError, TypeError) as e:
        if is_pybind11_function(obj):
            return get_pybind11_signature(obj)
        print(f"inspect.signature({obj}) caused error: {e}")
        return [{'name': 'builtin_method', 'type': 'unknown', 'default': None}], 'unknown'
    except Exception as e:
        print(f"Error determining signature for {obj}: {e}")
        return [{'name': 'unknown', 'type': 'unknown', 'default': None}], 'unknown'


def override_module_calls_with_janus(module):
    module_name = module.__name__
    modules_by_name[module_name] = module

    if not hasattr(module, '_original_functions'):
        module._original_functions = {}
    original_functions = module._original_functions

    for name, original_function in inspect.getmembers(module, inspect.isroutine):
        if name in original_functions:
            continue
        original_functions[name] = original_function

        func = original_function.__func__ if hasattr(original_function, '__func__') else original_function

        param_details, return_type = signature(original_function)

        param_names = [param['name'] for param in param_details]
        param_types = [str(param['type']) for param in param_details]
        param_defaults = [param['default'] for param in param_details]

        full_function_name = f"{module_name}_{name}"

        try:
            janus.cmd('user', 'register_function', full_function_name, param_names, param_types, param_defaults, str(return_type))
        except Exception as e:
            print(f"Error registering function {name} with Prolog: {e}")

        def make_overridden_function(original_function, full_function_name):
            @wraps(original_function)
            def overridden_function(*args, **kwargs):
                pargs = list(args)
                try:
                    result_iter = janus.apply_once('user', 'from_python', full_function_name, args, pargs, kwargs)
                    result_list = list(result_iter)
                    if len(result_list) == 1:
                        result = result_list[0]
                        # if str(result) == "None": return None
                        if result is not None:
                            return result
                        return None
                    return original_function(*args, **kwargs)

                except Exception as e:
                    traceback.print_exc()
                    raise e

            return overridden_function

        overridden_func = make_overridden_function(original_function, full_function_name)
        setattr(module, name, overridden_func)

def test_my_module():
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
        """Calculates the factorial of n recursively."""
        if n == 0:
            return 1
        else:
            return -n  # Intentionally incorrect
    
    # Assign these functions to the module
    my_module.add = add
    my_module.multiply = multiply
    my_module.greet = greet
    my_module.factorial = factorial

    # override the module calls with prolog
    override_module_calls_with_janus(my_module)

    # Test calls
    print("Testing overridden functions:")
    print(f"add(2, 3): {my_module.add(2, 3)}")
    print(f"multiply(2, 3): {my_module.multiply(2, 3)}")
    print(f"greet('Alice'): {my_module.greet('Alice')}")
    print(f"factorial(5): {my_module.factorial(5)}")



def test_hyperon_overrides():
    import hyperonpy
    import hyperon

    override_module_calls_with_janus(hyperonpy)

    metta = hyperon.runner.MeTTa()
    print(metta.run("!(+ 1 1)"))


if __name__ == "__main__":
    import sys
    import os

    sys.path.append(os.path.dirname(os.path.abspath(__file__)))

    # Initialize Janus
    prolog_module_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'metta_corelib.pl')
    consult_command = f"consult('{prolog_module_path}')"
    janus.query_once(consult_command)


    #test_hyperon_overrides()
    test_my_module()
