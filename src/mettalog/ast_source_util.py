#!/usr/bin/env python3

import sys
import os
import importlib.util
import importlib
import inspect
import types
import inspect
import ast
from typing import *
from typing import List, Dict, Set, Callable
from typing_extensions import *
from typing import get_type_hints
# For environments where typing_extensions is available, it can help with newer typing constructs.
try:
    from typing_extensions import *
    from typing_extensions import get_origin, get_args
except ImportError:
    def get_origin(tp):
        return getattr(tp, '__origin__', None)

    def get_args(tp):
        return getattr(tp, '__args__', ())

# Attempt to import astor for versions of Python < 3.9
try:
    import astor
except ImportError:
    astor = None

def ast_src(ast_node, indent=4, use_dump=False):
    """Convert an AST node to source code or dump its structure."""
    if hasattr(ast, 'unparse') and not use_dump:
        try:
            return ast.unparse(ast_node)
        except Exception:
            # Fallback to using ast.dump if unparse fails or use_dump is True
            pass
    if astor:
        return astor.to_source(ast_node)

    return ast.dump(ast_node, indent)


def read_source_from_path(file_path):
    try:
        with open(file_path, "r") as file:
            return file.read()
    except Exception as e:
        return None, f"An error occurred while reading the file: {e}"

def get_member_ast_from_ast(src_ast, member_name):
    for node in ast.walk(src_ast):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Assign)) and node.name == member_name:
            return ast
    return src_ast


# also make sure all these work
# Example usage for different types of inputs
# print(get_src(ast.parse("x = 1")))  # For an AST object
# print(get_src(os))  # For a module
# print(get_src(os.path.join))  # For a function
# print(get_src("/path/to/your_script.py"))  # For a file path
# print(get_src("/path/to/your_script.py","function_name"))
# print(get_src("/path/to/your_script.py", "class_name"))
# print(get_src(os.path))
# print(get_src("os.path","join"))
# print(get_src(os.path,"join"))
# if you cant ge thte src  member/mebers.. use the extract_function_properties and try to make it up
def get_src(input_obj, member_name=None):
    try: 
        return get_src0(input_obj, member_name)
    except Exception as e:
        print(type(input_obj))
        print(f"Error: {e}")

def get_src0(input_obj, member_name=None):
    """Extract source code from various types of inputs."""
    if isinstance(input_obj, (list, tuple)) and len(input_obj) == 2:
        input_obj, member_name = input_obj

    if member_name and not isinstance(member_name, str):
        member_name = str(member_name)

    # If input_obj is an AST, unparse it to get the source code
    if isinstance(input_obj, ast.AST):
        if member_name:
            member_ast = get_member_ast_from_ast(input_obj, member_name)
            if member_ast:
                return ast_src(member_ast)

            print(f"Member '{member_name}' not found in AST.")
        return ast_src(input_obj)

    
    maybe_path = str(input_obj)

    # Check if the input is a path to a Python file
    if maybe_path.endswith(".py") and os.path.isfile(maybe_path):
        with open(maybe_path, "r") as file:
            source_code = file.read()
            if member_name:
                parsed_ast = ast.parse(source_code)
                member_ast = get_member_ast_from_ast(parsed_ast, member_name)
                if member_ast:
                    return ast_src(member_ast)
                else:
                    return f"Member '{member_name}' not found in '{maybe_path}'."
            return source_code

    # Someone passed us a string that might be code
    if maybe_path == input_obj:
        if member_name:
            parsed_ast = ast.parse(input_obj)
            member_ast = get_member_ast_from_ast(parsed_ast, member_name)
            if member_ast:
                return ast_src(member_ast)
            else:
                return f"Member '{member_name}' not found in provided source."
        return input_obj  # Assuming the string is source code if no member_name is given


    # Direct function, method, class, or module object
    if isinstance(input_obj, (types.FunctionType, types.MethodType)):
        if member_name is None:
            source_code = inspect.getsource(input_obj)
            if source_code is not None: 
                return source_code
            else:
                return guess_method_src_from_properties(input_obj)


    # Direct module object
    if isinstance(input_obj, ( types.ModuleType)):
        if member_name is None:
            source_code = inspect.getsource(input_obj)
            if source_code is not None: 
                return source_code
            else:
                return guess_module_src_from_properties(input_obj)

    # Handle module or member name provided as a string
    if member_name:
        module = __import__(as_str(input_obj), fromlist=[member_name] if member_name else [])
        member = getattr(module, member_name, None)
        if member:
            return get_src(member)
        else:
            return f"Member '{member_name}' not found in provided {input_obj}"
    else:
        module = __import__(as_str(input_obj))
        return get_src(module)

def as_str(obj):
    if isinstance(obj, str): 
        return str
    res = getattr(obj, '__name__', None)
    if res:
        return res
    return repr(obj)

def as_comments(props):
    """Convert a dictionary of properties to a string of comment lines."""
    return '\n'.join([f"# {key}: {value}" for key, value in props.items()])

def guess_method_src_from_properties(func):
    """Guess and reconstruct the source code from the properties of a function."""
    signature = inspect.signature(func)
    type_hints = get_type_hints(func)
    props = {
        "name": func.__name__,
        "module": func.__module__,
        "return_type": type_hints.get('return', signature.return_annotation).__name__ if hasattr(type_hints.get('return', signature.return_annotation), '__name__') else str(type_hints.get('return', signature.return_annotation)),        
        "annotations": {k: v.__name__ for k, v in func.__annotations__.items()} if func.__annotations__ else 'None',
        "method_type": method_type(func),
        "is_coroutine": inspect.iscoroutinefunction(func),
        "file_location": inspect.getfile(func) if hasattr(inspect, 'getfile') else None,
    }
    src = generate_function_signature(func) + '\n    '
    src += repr(func.__doc__) if func.__doc__ else '""' + '\n    '
    src += as_comments(props).replace('\n', '\n    ') + '\n    '
    src += "return None"
    return src

import inspect
from typing import get_type_hints

def guess_class_src_from_properties(cls):
    """Guess and reconstruct the source code from the properties of a class."""
    class_name = cls.__name__
    module_name = cls.__module__
    doc_string = inspect.getdoc(cls)
    methods = inspect.getmembers(cls, predicate=inspect.isfunction)
    
    # Start constructing the class source code string
    src = f"class {class_name}:\n"
    src += f"    \"\"\"{doc_string}\"\"\"\n" if doc_string else ''
    
    # Process each method in the class
    for method_name, method in methods:
        method_sig = inspect.signature(method)
        type_hints = get_type_hints(method)
        method_type_indicator = method_type(method)
        
        # Generate method signature
        params = ', '.join(
            f"{name}: {type_hints.get(name, param.annotation).__name__ if hasattr(type_hints.get(name, param.annotation), '__name__') else str(type_hints.get(name, param.annotation))} = {repr(param.default)}"
            if param.default is not inspect.Parameter.empty else
            f"{name}: {type_hints.get(name, param.annotation).__name__ if hasattr(type_hints.get(name, param.annotation), '__name__') else str(type_hints.get(name, param.annotation))}"
            for name, param in method_sig.parameters.items()
        )
        return_type = f" -> {method_sig.return_annotation.__name__}" if hasattr(method_sig.return_annotation, '__name__') else ''
        
        # Add method type (static, class, or instance method)
        if method_type_indicator == 'static':
            src += "    @staticmethod\n"
        elif method_type_indicator == 'class':
            src += "    @classmethod\n"
        
        # Add method definition
        src += f"    def {method_name}({params}){return_type}:\n"
        method_doc = inspect.getdoc(method)
        src += f"        \"\"\"{method_doc}\"\"\"\n" if method_doc else ''
        src += "        pass  # Original implementation removed\n\n"
    
    # Remove the last two newlines for cleanliness
    if src.endswith("\n\n"):
        src = src[:-2]

    # Return the constructed class source
    return f"# Class reconstructed from properties\n# Module: {module_name}\n{src}"



def guess_module_src_from_properties(module):
    """Attempt to get the source code of a module using inspect.getsource.
    Fall back to reconstructing source code from properties if necessary."""
    module_name = module.__name__
    src = f"# Module name: {module_name}\n"

    # Attempt to use inspect.getsource to get the module's source code
    try:
        return inspect.getsource(module)
    except Exception as e:
        src += f"# inspect.getsource failed: {e}\n"
    
    # If inspect.getsource fails, proceed to reconstruct source code from properties
    doc_string = inspect.getdoc(module)
    src += f"\"\"\"{doc_string}\"\"\"\n\n" if doc_string else ''
    
    # Iterate through all attributes of the module
    for attr_name, attr_value in inspect.getmembers(module):
        if inspect.isclass(attr_value):
            # Reconstruct class source
            class_src = guess_class_src_from_properties(attr_value)  # Assuming you have this function implemented
            src += class_src + "\n\n"
        elif inspect.isfunction(attr_value):
            # Reconstruct function source
            function_src = guess_source_from_properties(attr_value)  # Assuming you have this function implemented
            src += function_src + "\n\n"
        elif isinstance(attr_value, (types.BuiltinFunctionType, types.BuiltinMethodType)):
            # Skip built-in functions and methods
            continue
        elif isinstance(attr_value, (int, float, str, list, dict, set, tuple)):
            # Add simple data types directly
            src += f"{attr_name} = {repr(attr_value)}\n"
        else:
            src += f"#{attr_name} = {repr(attr_value)}\n"
        

    # Trim trailing newlines for cleanliness
    src = src.rstrip('\n')
    
    # Return the constructed module source
    return src

# Usage example:
# import some_module
# print(guess_module_src_from_properties(some_module))


def src_ast(module_name_or_source_code):
    try:
        source_code = get_src(module_name_or_source_code)
        # Parse the source code into an AST
        src_ast = ast.parse(source_code)
        return src_ast
    except ImportError as e:
        print(f"Error: Module {module_name_or_source_code} not found. {e}")
    except Exception as e:
        print(f"An error occurred: {e}")

def src_ast_str(source_code):
    try:
        # Parse the source code into an AST
        src_ast = src_ast(source_code)
        return ast_src(src_ast, indent=4)
    except ImportError as e:
        print(f"Error: Module {source_code} not found. {e}")
    except Exception as e:
        print(f"An error occurred: {e}")


def print_ast(module_name_or_source_code) -> ast.AST:
    try:
        # Parse the source code into an AST
        src_ast = src_ast(module_name_or_source_code)
        # Use ast.dump to get a formatted string of the AST
        print(ast.dump(src_ast, indent=4))
        return src_ast
    except ImportError as e:
        print(f"Error: Module {module_name} not found.")
    except Exception as e:
        print(f"An error occurred: {e}")


def resolve_annotation(annotation, type_hints):
    """Resolve annotations, including forward references and complex types."""
    if isinstance(annotation, str):
        # Resolve forward references
        return type_hints.get(annotation, annotation)
    return annotation

def format_annotation(annotation, type_hints):
    """Format annotation to a readable string, handling various typing constructs."""
    annotation = resolve_annotation(annotation, type_hints)
    origin = get_origin(annotation)
    if origin is not None:
        base = origin.__name__ if origin else str(annotation)
        if base == "Union":
            base = "typing.Union"  # Ensure Union is prefixed for clarity
        args = ', '.join([format_annotation(arg, type_hints) for arg in get_args(annotation)])
        return f"{base}[{args}]"
    elif hasattr(annotation, '__name__'):
        return annotation.__name__
    return str(annotation)

def format_default_value(value):
    """Format default values, handling complex cases including ellipsis and callables."""
    if value is inspect.Parameter.empty:
        return ''
    elif value is Ellipsis:
        return '=...'
    elif callable(value):
        # Handle callable defaults, using function name
        return f"={value.__name__}"
    elif isinstance(value, (str, int, float, bool, type(None))):
        # Direct types that can be accurately represented
        return f"={repr(value)}"
    # Fallback for more complex types
    return f"={repr(value)}"

def generate_function_signature(func):
    """Generate a comprehensive function signature string."""
    sig = inspect.signature(func)
    type_hints = get_type_hints(func, globalns=globals(), localns=locals())

    sig_parameters = []
    for name, param in sig.parameters.items():
        annotation = type_hints.get(name, param.annotation)
        param_type_str = f": {format_annotation(annotation, type_hints)}" if annotation != inspect.Parameter.empty else ''
        default_value = format_default_value(param.default)
        parameter_definition = f"{name}{param_type_str}{default_value}"
        sig_parameters.append(parameter_definition)

    return_type_annotation = type_hints.get('return', sig.return_annotation)
    return_type = f"-> {format_annotation(return_type_annotation, type_hints)}" if return_type_annotation not in [None, inspect.Parameter.empty] else ''

    param_str = ", ".join(sig_parameters)
    function_name = func.__name__
    signature_representation = f"def {function_name}({param_str}) {return_type}:"

    return signature_representation


def method_type(func):
    """Determine the method type (instance, class, or static)."""
    if isinstance(func, staticmethod):
        return "static"
    elif isinstance(func, classmethod):
        return "class"
    else:
        # Inspect signature for 'self'
        sig = inspect.signature(func)
        if 'self' in sig.parameters:
            return "instance"
        return "function"  # Fallback for regular functions


def extract_function_properties(func):
    signature = inspect.signature(func)
    type_hints = get_type_hints(func, globalns=globals(), localns=locals())
    # Extracting parameter details    

    # Additional properties to extract
    func_props = {
        "name": func.__name__,
        "module": func.__module__,
        "signature": generate_function_signature(func),
        "return_type": type_hints.get('return', signature.return_annotation),        
        "docstring": func.__doc__,
        "annotations": func.__annotations__,
        "method_type": method_type(func),  # Determine the method type
        "is_coroutine": inspect.iscoroutinefunction(func),
        "source_code": get_src(func),
        "file_location": inspect.getfile(func) if hasattr(inspect, 'getfile') else None,
        "ast": ast.parse(get_src(func)),
    }

    return func_props

def example_function(name: str, age: int | str = 30, hobbies: list[str] = None) -> str:
    """Example function with Union and List type hints and default values."""
    return f"{name} is {age} years old with hobbies {hobbies}."

# Example of a complex function signature to test
def example_function2(name: str, age: int = 30, is_student: bool = False, hobbies: list[str] = None, callback: Callable = lambda x: x) -> int | str:
    """Complex function with mixed types and default values."""
    return f"{name}, age {age}, is {'a student' if is_student else 'not a student'}. Hobbies: {hobbies}, callback: {callback('test')}"

# Example class to demonstrate method type detection
class ExampleClass:
    def instance_method(self, x):
        """An instance method."""
        return x

    @classmethod
    def class_method(cls, x):
        """A class method."""
        return x

    @staticmethod
    def static_method(x):
        """A static method."""
        return x

# Additional Example Usages
def function_sig_tests():
    # Extract and display the function properties
    function_properties = extract_function_properties(example_function)
    for key, value in function_properties.items():
        print(f"{key}: {value}")

    # Extract and display the properties of the second function
    function_properties2 = extract_function_properties(example_function2)
    for key, value in function_properties2.items():
        print(f"{key}: {value}")

    return
    # Extract and display the properties of each method in ExampleClass
    for method_name in ['class_method', 'static_method','instance_method']:
        method = getattr(ExampleClass, method_name)
        props = extract_function_properties(method)
        print(f"Properties of {method_name}:")
        for key, value in props.items():
            print(f"  {key}: {value}")
        print()

TH="\n----------------------------------\n"

# Additional code to handle files, modules, functions as before
def ast_tests():
    

    print(f"{TH}AST Object Source{TH}")
    try:
        print(get_src(ast.parse("x = 1")))  # For an AST object
    except Exception as e:
        print(f"Error: {e}")

    print(f"{TH}Module Source (os){TH}")
    try:
        print(get_src(os))  # For a module
    except Exception as e:
        print(f"Error: {e}")

    print(f"{TH}Function Source (os.path.join){TH}")
    try:
        print(get_src(os.path.join))  # For a function
    except Exception as e:
        print(f"Error: {e}")


    example_script_path=f"{__file__}"

    print(f"{TH}File Source ('example_script.py'){TH}")
    try:
        print(get_src(example_script_path ))  # For this file path
    except Exception as e:
        print(f"Error: {e}")

    print(f"{TH}Specific Function Source from File ('example_script.py', 'get_src'){TH}")
    try:
        print(get_src(example_script_path , "get_src"))  # For a function in this file
    except Exception as e:
        print(f"Error: {e}")

    print(f"{TH}Module Source (os.path){TH}")
    try:
        print(get_src(os.path))  # For a module
    except Exception as e:
        print(f"Error: {e}")


def ast_tests_sm():
    print(f"{TH}Module Member Source ('os.path', 'join'){TH}")
    try:
        print(get_src("os.path", "join"))  # For a module member
    except Exception as e:
        print(f"Error: {e}")

    print(f"{TH}Module Member Source (os.path, 'join'){TH}")
    try:
        print(get_src(os.path, "join"))  # For a module member
    except Exception as e:
        print(f"Error: {e}")



if __name__ == "__main__":
    ast_tests()
    function_sig_tests()
