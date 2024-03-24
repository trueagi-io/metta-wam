import traceback
import importlib
import importlib.util
import os
import inspect
import sys

from hyperon import MeTTa

the_python_runner = MeTTa()
class MettaLearner:
    pass  # Use 'pass' as a placeholder for an empty class body
mettaLearner = MettaLearner()
def use_mettalog():
    print("hi!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

def override_metta(meTTa):
    replace_space_rust_functions()

def print_exception_stack(e):
    print(f"Exception occurred: {e}")
    traceback.print_exc()

def load_module(name_or_path):
    before = set(sys.modules.keys())
    module = None  # Initialize module variable

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
    new_modules = {name: sys.modules[name] for name in after}
    return module, new_modules

def update_dict_from_function(func, current_dict):
    #print(f"Processing function: {func.__name__}")
    sig = inspect.signature(func)
    params = sig.parameters
    if len(params) == 0:
        result = func()
    elif len(params) == 1:
        result = func(the_python_runner)
    else:
        return current_dict  # Skip functions requiring more than one argument
    print(result)
    #if isinstance(result, dict):
    current_dict.update(result)
    return current_dict

def process_module(module, combined_dict1):
    members = inspect.getmembers(module)
    for name, obj in members:
        # Check if the object is a function and has the desired suffix or attribute
        if inspect.isfunction(obj) and (name.endswith('_ops') or name.endswith('_atoms') or name.endswith('_tokens') or hasattr(obj, 'register_atoms_internal')                                            or hasattr(obj, 'register_atoms')):
            print(f"Found function matching criteria: {module}.{obj}")
            combined_dict1 = update_dict_from_function(obj, combined_dict1)
            #combined_dict1.update(obj())
    #print(combined_dict1)
    return combined_dict1


def load_functions0(name_or_path):
    combined_dict = {}
    main_module, new_modules = load_module(name_or_path)

    #if main_module:  # Check if main_module was successfully loaded
    print(f"Processing main module: {main_module.__name__}")
    combined_dict = process_module(main_module, combined_dict)

    for module in new_modules.values():
        #print(f"Processing new module: {module.__name__}")
        combined_dict = process_module(module, combined_dict)

    return combined_dict



def load_functions(name_or_path):
    combined_dict = {}
    combined_dict.update(load_functions0("motto.sparql_gate"))
    combined_dict.update(load_functions0("motto.llm_gate"))
    #      load_functions0("motto.sparql_gate")
    combined_dict.update(load_functions0(name_or_path))
    return combined_dict;



