import traceback
import importlib
import importlib.util
import os
import inspect
import sys
import os
from pathlib import Path

from hyperon import MeTTa


# Step 1: Define the verbosity level (could be set from environment or manually)
METTALOG_VERBOSE = int(os.getenv('METTALOG_VERBOSE', 0))  # Default to 0 if not set

def log_message(message, level=1):
    """
    Logs a message based on the current verbosity level.

    Args:
        message (str): The message to log.
        level (int): The verbosity level for this message.
                     0 = silent, 1 = user, 2 = developer debug, 3 = tracing.
    """
    if METTALOG_VERBOSE >= level:
        print(message)

# Example usage

def some_function():
    log_message("This is a user-level message", 1)
    log_message("This is a developer debug message", 2)
    log_message("This is a tracing message", 3)

# You can then change METTALOG_VERBOSE based on the context of the script
# METTALOG_VERBOSE = 0  # Silent
# METTALOG_VERBOSE = 1  # User messages
# METTALOG_VERBOSE = 2  # Debug messages
# METTALOG_VERBOSE = 3  # Trace-level messages



def print_exception_stack(e):
    print(f"Exception occurred: {e}")
    traceback.print_exc()


#try:
    def load_module(name_or_path):
        before = set(sys.modules.keys())
        module = None  # Initialize module variable

        try:
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
        except Error as e:
            print(f"Skipping due to a missing dependency: {e}")
            print_exception_stack(e)

        after = set(sys.modules.keys())
        new_modules = {name: sys.modules[name] for name in after - before if name in sys.modules}

      #  print(new_modules)  # This will print out the newly loaded modules, if any

        return module, new_modules

    def update_dict_from_function(func, current_dict):
        try:
            sig = inspect.signature(func)
            params = sig.parameters
            if len(params) == 0:
                result = func()
            elif len(params) == 1:
                result = func(the_python_runner)
            else:
                return current_dict  # Skip functions requiring more than one argument

            if isinstance(result, dict):
                current_dict.update(result)
        except Error as e:
            print(f"Function {func.__name__} skipped due to missing dependency: {e}")
            print_exception_stack(e)

        return current_dict

    def load_functions(name_or_path):
        # Load the module and capture any new modules that were loaded with it
        main_module, new_modules = load_module(name_or_path)

        # Dictionary to store combined results
        combined_dict = {}

        # Function to safely process modules
        def process_module(module, combined_dict):
            try:
                # Safely get all members of the module
                members = inspect.getmembers(module)
            except Exception as e:
                print_exception_stack(e)
                print(f"Error while getting members from module {getattr(module, '__name__', 'unknown')}: {e}")
                
                members = []  # Continue with empty members if error occurs

            # Iterate through all retrieved members
            for name, obj in members:
                if inspect.isfunction(obj) and hasattr(obj, 'register_atoms'):
                    try:
                        # Execute the function and update the combined dictionary
                        combined_dict = update_dict_from_function(obj, combined_dict)
                    except Exception as e:
                        # Print error message but continue processing other functions
                        print_exception_stack(e)
                        print(f"Error while executing function {name} in module {getattr(module, '__name__', 'unknown')}: {e}")
                        
            return combined_dict

        # Process auxiliary modules first
        for module in new_modules.values():
            combined_dict = process_module(module, combined_dict)

        # Process the main module last
        if main_module:  # Check if main_module was successfully loaded
            combined_dict = process_module(main_module, combined_dict)

        return combined_dict

    try:
        the_python_runner = MeTTa()
    except Exception as e:
        print_exception_stack(e)

    class MettaLearner:
        pass  # Use 'pass' as a placeholder for an empty class body

    mettaLearner= MettaLearner()

#except Exception as e:
#    print_exception_stack(e)

