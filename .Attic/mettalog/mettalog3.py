import traceback
import importlib
import importlib.util
import os
import inspect
from hyperon import *  # Import everything from hyperon
from motto import *  # Import everything from motto
from inspect import getmembers, isfunction  # Specific imports from inspect already included

the_python_runner = MeTTa()

def print_exception_stack(e):
    print(f"Exception occurred: {e}")
    traceback.print_exc()

def load_module(name_or_path):
    before = set(sys.modules.keys())  # Fixing initialization
    if name_or_path.endswith('.py'):
        file_path = os.path.abspath(name_or_path)
        module_name = os.path.splitext(os.path.basename(file_path))[0]
        spec = importlib.util.spec_from_file_location(module_name, file_path)
        if spec is None:
            raise ImportError(f"Could not load module from {file_path}")
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
    else:
        module = importlib.import_module(name_or_path)

    after = set(sys.modules.keys())
    new_modules = {name: sys.modules[name] for name in after - before}  # Fixing new modules determination
    return module, new_modules  # Fixing return value

def update_dict_from_function(func, current_dict):
    #print(f"Processing function: {func}")
    sig = inspect.signature(func)
    params = sig.parameters
    if len(params) == 0:
        result = func()
    elif len(params) == 1:
        result = func(the_python_runner)
    else:
        print(f"Skipping function {func.__name__}: too many parameters")
        return current_dict  # Proper handling of the function with too many parameters

    if isinstance(result, dict):
        for key, value in result.items():
            print(f"{key}: {value}")
        current_dict.update(result)
    else:
        print(f"Result from {func.__name__} is not a dictionary, skipping update.")
    return current_dict

def process_module(module, combined_dict):
    print(f"Processing module: {module.__name__}")
    members = getmembers(module, isfunction)

    # Define the specific suffixes you are interested in
    specific_suffixes = ['_ops', '_atoms', '_tokens']

    for name, obj in members:
        # Check if the function name ends with any of the specific suffixes
        if any(name.endswith(suffix) for suffix in specific_suffixes) or hasattr(obj, 'register_atoms'):
            print(f"Found function {name} with specific criteria")
            combined_dict = update_dict_from_function(obj, combined_dict)

    return combined_dict



def list_functions_with_decorators_and_annotations(module):
    print(f"Listing functions, decorators, and annotations in module: {module.__name__}")
    members = getmembers(module, isfunction)
    for name, func in members:
        print(f"Function name: {name}")

        # Attempt to identify decorators by known attributes they add
        # Replace 'decorator_attribute' with actual attributes added by your decorators
        known_decorator_attributes = ['register_atoms', 'register_tokens', 'decorator_attribute3']
        decorators = [attr for attr in known_decorator_attributes if hasattr(func, attr)]
        if decorators:
            print("    Decorators: " + ", ".join(decorators))
        #else:
            #print("    No known decorators")

        # Print annotations
        if func.__annotations__:
            for param, annotation in func.__annotations__.items():
                print(f"    Annotation - {param}: {annotation}")
        #else:
        #    print("    No annotations")

    print("End of list for module:", module.__name__)


def list_functions_in_module(module):
    print(f"Listing functions in module: {module.__name__}")
    members = getmembers(module, isfunction)
    for name, obj in members:
        print(f"Function name: {name}")
    print("End of list for module:", module.__name__)

def load_functions0(name_or_path):
    main_module, new_modules = load_module(name_or_path)
    combined_dict = {}

    # List and process functions in the main module
    list_functions_with_decorators_and_annotations(main_module)
    print(f"Processing main module: {main_module.__name__}")
    combined_dict = process_module(main_module, combined_dict)

    # List and process functions in all new modules
    for module in new_modules.values():
        list_functions_with_decorators_and_annotations(module)
        print(f"Processing new module: {module.__name__}")
        combined_dict = process_module(module, combined_dict)

    return combined_dict


def load_functions(module_path):
    dict1 = load_functions0(module_path)
    # Optionally add more modules to process here
    return dict1

class MettaLearner:
    pass  # Placeholder for MettaLearner class

mettaLearner = MettaLearner()

def use_mettalog():
    pass  # Placeholder for use_mettalog function

# Example of usage
#load_functions("motto.sparql_gate")  # Replace "llm_gate" with actual module name or path
#load_functions("motto.llm_gate")  # Replace "llm_gate" with actual module name or path
#load_functions("motto")  # Replace "llm_gate" with actual module name or path
#load_functions("hyperon.ext") 
#load_functions("hyperon.stdlib") 


