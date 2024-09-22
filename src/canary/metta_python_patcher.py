#!/usr/bin/env python3
# Filename: metta_python_patcher.py
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

# Global variables
global runner
runner = None
global norepl_mode
norepl_mode=True
global catom_mode
catom_mode=False
global cb
cb = None
global suspend_trace
suspend_trace = False
global rust_mode
rust_mode = False  # Set to False as default

# ANSI color code for cyan
CYAN = "\033[96m"
RESET = "\033[0m"

def commentary(message):
    """Function to print a message in cyan for debug commentary."""
    print(f"{CYAN}{message}{RESET}")

# Log messages based on verbosity level
def mesg(level, message='', m2=None, m3=None):
    if not isinstance(level, int):
        message = f"{level} {message}"
        level = TRACE

    message = f"{message} {m2 or ''} {m3 or ''}"

    print(message)
    return
    if METTALOG_VERBOSE >= level:
        print(message)

def set_no_mock_objects_flag(flag: bool):
    global mock_throw_error
    mock_throw_error = flag

def get_no_mock_objects_flag() -> bool:
    return mock_throw_error

# Set verbosity level
def set_verbosity(level):
    global METTALOG_VERBOSE
    newlevel = int(level)
    if isinstance(newlevel, int):
        METTALOG_VERBOSE = newlevel
        mesg(DEBUG, f"Verbosity set to level {newlevel}")
    else:
        print(f"Invalid verbosity level '{level}' provided. Keeping level: {METTALOG_VERBOSE}.")

if True:
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
                    print(f"Verbosity level defaulting to {METTALOG_VERBOSE}.")

        mesg(DEBUG, f"Argv={sys.argv}")
    except Exception as e:
        mesg(USER, f"An error occurred: {e}")
        if METTALOG_VERBOSE >= DEBUG:
            traceback.print_exc()


def name_dot(module, name):
    if module is None:
        return name
    return f"{module.__name__}.{name}"

def signature(obj):
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


def ignore_exception(*args, **kwargs):
    pass

# Define Enums for member types, levels, and implementation status
class MemberType(Enum):
    VARIABLE = "variable"
    PROPERTY = "property"
    METHOD = "method"
    STATIC_METHOD = "staticMethod"
    CLASS_METHOD = "classMethod"
    FUNCTION = "function"
    MODULE = "module"
    CLASS = "class"
    SPECIAL_METHOD = "specialMethod"
    SPECIAL_VARIABLE = "specialVariable"

class MemberLevel(Enum):
    INSTANCE = "instance"  # For instance methods and properties
    CLASS = "class"        # For class methods, static methods, and class variables
    MODULE = "module"      # For module-level members

class Implemented(Enum):
    LOCAL = "Local"          # Implemented in the current class/module
    INHERITED = "Inherited"  # Inherited from a base class
    DEFAULT = "Default"      # Inherited from 'object'

# Observer Class
class Observer:
    def __init__(self):
        self.subscribers = {}

    def subscribe(self, event_type, callback, level="instance"):
        """Override to an event type ('before_call', 'after_call', 'set', 'get') with a callback."""
        key = f"{level}_{event_type}"
        if key not in self.subscribers:
            self.subscribers[key] = []
        self.subscribers[key].append(callback)

    def notify(self, func, event_type, *args, **kwargs):
        """Notify all overriders for a specific event type."""
        global suspend_trace
        #if suspend_trace:
        #   return

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
        final_result = None  # Store the first valid result here

        # Check if there are any subscribers overrided to this event key
        if event_key in self.subscribers:
            for callback in self.subscribers[event_key]:
                result = callback(*args)
                # Apply the first valid override (with 'overriden_value' or 'do_not_really_*')
                if result and ('return_value' in result or 'do_not_really_set' in result):
                    # If we don't have a final result yet, accept this one
                    if final_result is None:
                        final_result = result
                # Notify others even if we already have a valid result

        # Return the first valid result, or None if no override was found
        return final_result or kwargs.get('default_value')

# 3. Monkey Patching Class
class Overrider:


    def __init__(self, all=True):
        self.observer = Observer()
        self.attempted_to_capture_init_variables_on_classes = set()
        self.marked_classes = set()
        self.marked_members = set()
        self.class_hierarchy = {}
        self.marked_member_info_by_classname = {}
        self.patched_members = {}
        self.set_default_observers(all, all)

    # Setup observer and monkey-patcher system
    def set_default_observers(self, allDefaults, also_static):   
        """Override events for method calls, property access, etc."""

        # Override to events
        def before_call_callback(*args):
            target_class, method_name = args[0], args[1]
            mesg(f"[Overrider] Before calling {name_dot(target_class, method_name)}{args[2:]}")

        def after_call_callback(*args):
            target_class, method_name, result = args[0], args[1], args[2]
            mesg(f"[Overrider] After calling {name_dot(target_class, method_name)} -> {result}")

        def get_callback(*args):
            target_class, property_name, current_value = args[0], args[1], args[2]
            mesg(f"[Overrider] Getting {name_dot(target_class, property_name)}, current value: {current_value}")

        def set_callback(*args):
            target_class, property_name, new_value = args[0], args[1], args[2]
            mesg(f"[Overrider] Setting {name_dot(target_class, property_name)} to {new_value}")

        if allDefaults:
            
            self.add_override('before_call', before_call_callback, level='instance')
            self.add_override('after_call', after_call_callback, level='instance')
            self.add_override('get', get_callback, level='instance')
            self.add_override('set', set_callback, level='instance')

            if also_static:
                self.add_override('before_call', before_call_callback, level='class')
                self.add_override('after_call', after_call_callback, level='class')
                self.add_override('get', get_callback, level='class')
                self.add_override('set', set_callback, level='class')

            self.add_override('before_call', before_call_callback, level='module')
            self.add_override('after_call', after_call_callback, level='module')
            self.add_override('get', get_callback, level='module')
            self.add_override('set', set_callback, level='module')

    def check_override(self, func, event_type, *args, **kwargs):
        return self.observer.notify(func, event_type, *args, **kwargs)

    def add_override(self, event_type, callback, level="instance"):
        return self.observer.subscribe(event_type, callback, level)

    def mark_members(self, obj, also_static=False):
        if obj is not None:
            if inspect.isclass(obj): 
                mark_class(self, obj, also_static)
            elif inspect.ismodule(obj): 
                monkey_patch_module(self, obj)
            else:
                mark_class(self, type(obj))
                

    def mark_class(self, cls, also_static=False, also_inherited=False):
        if not inspect.isclass(cls):
            mesg(f"Warning: {cls} is not a class. Skipping.")
            return
        cls_name = cls.__name__
        if cls_name in self.marked_classes:
            return
        mesg(f"Inspecting {cls_name}:")
        self.class_hierarchy[cls_name] = [base for base in cls.__bases__ if base is not object]
        self.marked_classes.add(cls_name)

        members = inspect.getmembers(cls)
        member_list = []

        for name, member in members:
            if name.startswith('_') and not (name.startswith('__') and name.endswith('__')):
                continue

            implemented_from = implemented_in(name, cls)
            is_callable = callable(member)
            self_usage = uses_self(member) if is_callable else False

            member_type = classify_member_type(cls, name, member)
            member_level = classify_member_level(cls, name, member, implemented_from, self_usage)
            implementation = classify_implementation(cls, implemented_from)

            # we skip these implicitly
            if member_level == MemberLevel.CLASS and member_type == MemberType.SPECIAL_VARIABLE:
                continue

            # skip static ?
            if member_level == MemberLevel.CLASS and not also_static:
                continue

            # skip inherited ?
            if implementation != Implemented.LOCAL.value and not also_inherited:
                continue

            if (cls_name, name) in self.marked_members:
                continue
            self.marked_members.add((cls_name, name))

            member_info = {
                'level': member_level,
                'implemented': implementation,
                'member_type': member_type,
                'name': name,
                'class_name': cls_name,
                'class_object': cls,
                'member': member,                
                'implemented_from': implemented_from,
            }
            member_list.append(member_info)

        if cls_name not in self.marked_member_info_by_classname:
            self.marked_member_info_by_classname[cls_name] = []
        self.marked_member_info_by_classname[cls_name].extend(member_list)
        self.print_marked(member_list)

    def print_marked(self, member_list):

        sorted_members = sorted(
            member_list,
            key=lambda x: (
                x['level'].value,
                x['member_type'].value,
                x['name']
            )
        )
        for member in sorted_members:
            if callable(member['member']):
                sig = signature(member['member'])
                mesg(f"{member['class_name']}: "
                     f"{{level: {member['level'].value}, "
                     f"member-type: {member['member_type'].name}, "
                     f"name: {member['name']}, "
                     f"signature: {sig}}}")
            else:
                mesg(f"{member['class_name']}: "
                     f"{{level: {member['level'].value}, "
                     f"member-type: {member['member_type'].name}, "
                     f"name: {member['name']}}}")

    def mark_base_classes(self, also_static=False, also_inherited=False):
        for class_name in list(self.class_hierarchy.keys()):
            bases = self.class_hierarchy[class_name]
            for base in bases:
                base_name = base.__name__
                if base_name not in self.marked_classes:
                    mesg(f"Inspecting base class {base_name}:")
                    self.mark_class(base, also_static=also_static, also_inherited=also_inherited)

    def monkey_patch_members(self):
        """Monkey-patch captured members using the Overrider."""
        for class_name, member_infos in self.marked_member_info_by_classname.items():
            #print(f"member_infos={member_infos}")
            cls = member_infos[0]['class_object']  # All members belong to the same class
            self.patch_class_init(cls)
            for member_info in member_infos:
                self.patch_member_info(cls, member_info)

    def create_member_info_and_patch_member(self, cls, member_name, member):
        """Create the member_info dictionary and patch the member."""

        # Determine if the member uses `self`, which can help identify its level
        is_callable = callable(member)
        uses_self_flag = uses_self(member) if is_callable else False

        # Determine the type and level of the member
        member_type = classify_member_type(cls, member_name, member)
        member_level = classify_member_level(cls, member_name, member, implemented_in(member_name, cls), uses_self_flag)

        # Create the member_info dictionary
        member_info = {
            'name': member_name,
            'member': member,
            'member_type': member_type,
            'level': member_level,
            'class_name': cls.__name__,
            'class_object': cls
        }

        # Call the patch function
        self.patch_member_info(cls, member_info)

    def patch_member_info(self, cls, member_info):
        """Helper function to patch an individual member."""
        name = member_info['name']
        #if name == '__init__': return
        member = member_info['member']
        level = member_info['level']
        member_type = member_info['member_type']

        qualified_name = f"{name_dot(cls,name)}"
        if callable(member):
            qualified_name = f"{qualified_name}{signature(member)}"

        if member in self.patched_members:
            return  # Skip if already patched

        self.patched_members[member] = member_info
        patcher = self

        try:
            if member_type == MemberType.METHOD:
                if level == MemberLevel.INSTANCE:
                    mesg(f"Patching instance method: {qualified_name}")
                    patcher.patch_instance_method(cls, name, member)
                elif level == MemberLevel.CLASS:
                    mesg(f"Patching static class method: {qualified_name}")
                    patcher.patch_class_method(cls, name, member)
                else:
                    mesg(f"Skipping method {qualified_name} with unknown level: {level}")
            elif member_type == MemberType.CLASS_METHOD:
                mesg(f"Patching (static) class method: {qualified_name}")
                patcher.patch_class_method(cls, name, member)
            elif member_type == MemberType.STATIC_METHOD:
                mesg(f"Patching static method: {qualified_name}")
                patcher.patch_static_method(cls, name, member)
            elif member_type == MemberType.PROPERTY:
                mesg(f"Patching property: {qualified_name}")
                # Retrieve the original property descriptor
                original_property = get_member_descriptor(cls, name)
                patcher.patch_instance_property(cls, name, original_property, level='instance')
            elif member_type == MemberType.VARIABLE:
                if level == MemberLevel.CLASS:
                    mesg(f"Patching static class variable: {qualified_name}")
                    patcher.patch_static_property(cls, name, member)
                elif level == MemberLevel.INSTANCE:
                    mesg(f"Patching instance variable: {qualified_name}")
                    patcher.patch_instance_property(cls, name, member, level='instance')
            elif member_type == MemberType.FUNCTION:
                # Functions inside classes are treated as methods
                if level == MemberLevel.INSTANCE:
                    mesg(f"Patching instance function: {qualified_name}")
                    patcher.patch_instance_method(cls, name, member)
                else:
                    mesg(f"Patching static class function: {qualified_name}")
                    patcher.patch_static_method(cls, name, member)
            elif member_type == MemberType.SPECIAL_METHOD:
                if level == MemberLevel.INSTANCE:
                    mesg(f"Patching special instance method: {qualified_name}")
                    patcher.patch_instance_method(cls, name, member)
                elif level == MemberLevel.CLASS:
                    mesg(f"Patching special class method: {qualified_name}")
                    patcher.patch_class_method(cls, name, member)
                else:
                    mesg(f"Skipping special method {qualified_name} with unknown level: {level}")
            else:
                mesg(f"Skipping unsupported member type: {member_type} for {qualified_name}")
        except Exception as e:
            mesg(f"Error patching {qualified_name}: {e}")

    def monkey_patch_module(self, module):
        """Monkey-patch functions and variables in a module."""
        patcher = self
        for name, obj in inspect.getmembers(module):
            if name.startswith('_'):
                continue

            try:
                if inspect.isfunction(obj) or inspect.isbuiltin(obj):
                    #mesg(f"Patching module function: {name_dot(module, name)}")
                    patcher.patch_module_function(module, name, obj)
                elif not callable(obj):
                    mesg(f"Borked Patching of module variable: {name_dot(module, name)}")
                    patcher.patch_module_property(module, name, obj)
            except Exception as e:
                mesg(f"Error patching {name_dot(module, name)}: {e}")


    def patch_class_init(self, cls):
        """Patch the class __init__ to handle instance variables."""
        original_init = cls.__init__
        patcher = self
    
        def capture_init_variables_once(instance, *args, **kwargs):
            """Wrapper for the class __init__ that patches instance variables."""
            mesg(f"Initializing {cls.__name__} with args {args} and kwargs {kwargs}")
            
            # Call the original __init__ method
            result = original_init(instance, *args, **kwargs)
            # undo this isntance variable system 
            setattr(cls, '__init__', original_init)            
            # Patch instance variables after __init__ completes
            patcher.patch_instance_variables(instance)
            return result
    
        # Ensure the method is callable and handle function vs bound method
        if cls not in self.attempted_to_capture_init_variables_on_classes:
            self.attempted_to_capture_init_variables_on_classes.add(cls)
            # Replace the original __init__ with capture_init_variables_once
            setattr(cls, '__init__', capture_init_variables_once)

    
    def patch_instance_variables(self, instance):
        """Patch instance variables by inspecting the instance's __dict__ after __init__ is called."""
        
        # Loop through all instance variables (in instance.__dict__)
        for var_name, value in instance.__dict__.items():
            # Use the patch_one_instance_variable to handle the patching logic
            self.patch_one_instance_variable(instance.__class__, var_name, value)
    
    def patch_one_instance_variable(self, cls, var_name, value):
        """Create the member_info dictionary and patch the instance variable."""
        
        # Assume all variables here are instance variables
        member_info = {
            'name': var_name,
            'member': value,
            'member_type': MemberType.VARIABLE,
            'level': MemberLevel.INSTANCE,
            'class_name': cls.__name__,
            'class_object': cls
        }
    
        # Call the patch function
        # global overrider
        self.patch_member_info(cls, member_info)

    def patch_instance_property(self, target_class, property_name, original_property, level):
        """Patch an instance property to allow observation of gets and sets."""
        try:
            mesg(f"Patching instance property: {property_name} in {target_class.__name__}")

            # Capture the observer from the current Overrider instance
            observer = self.observer
    
            # Create getter and setter functions using the captured observer
            def patched_getter(instance):
                global suspend_trace
    
                mesg(f"Calling patched_getter for '{property_name}' in {target_class.__name__}")
            
                #if suspend_trace and False:
                #    # If tracing is suspended, directly return the original value
                #    return getattr(instance, '__dict__', {}).get(property_name, None)
    
                try:
                    if not suspend_trace:
                        # Notify the observer and ensure a safe default dictionary is returned
                        suspend_trace = True
                        result = observer.notify(instance, 'get', target_class, property_name, original_property, level=level)
                        suspend_trace = False
                        if isinstance(result, dict) and result.get('do_not_really_get', False):
                            return result.get('return_value')



                    mesg(f"Accessing property '{property_name}' in {target_class.__name__} through patched_getter")
            
                    # Access the current value, handling __dict__ and original getter cases
                    if property_name in instance.__dict__:
                        current_value = instance.__dict__[property_name]
                    elif isinstance(original_property, property) and original_property.fget:
                        current_value = original_property.fget(instance)
                    else:
                        current_value = getattr(instance, property_name, None)
    
                    # Debug: Output the current value before notifying
                    mesg(f"[Debug] Current value of {property_name}: {current_value}")
            
    
                    #suspend_trace = False
            
                    # Return the original or modified value based on observer, falling back to current_value
                    return current_value
    
                except Exception as e:
                    suspend_trace = False  # Ensure tracing is resumed if an error occurs
                    mesg(f"Error getting property '{property_name}' in {target_class.__name__}: {e}")
                    raise
    
            def patched_setter(instance, new_value):
                global suspend_trace
    
                mesg(f"Calling patched_setter for '{property_name}' in {target_class.__name__} with value {new_value}")
    
                if suspend_trace:
                    instance.__dict__[property_name] = new_value
                    return

                try:
                    suspend_trace = True
                    # Notify the observer before setting the new value
                    result = observer.notify(instance, 'set', target_class, property_name, new_value, level=level)
                    if isinstance(result, dict):
                        if result.get('do_not_really_set', False): return
                        if 'return_value' in dict:
                            new_value = result.get('return_value')

                    # Set the value using the original setter if it exists, or directly update __dict__
                    if isinstance(original_property, property) and original_property.fset:
                        original_property.fset(instance, new_value)
                    else:
                        instance.__dict__[property_name] = new_value  # Directly set in __dict__
    
                    suspend_trace = False
    
                except Exception as e:
                    suspend_trace = False
                    mesg(f"Error setting property '{property_name}' in {target_class.__name__}: {e}")
                    raise
    
            # Apply the new property
            new_property = property(fget=patched_getter, fset=patched_setter)
            setattr(target_class, property_name, new_property)

            mesg(f"Successfully patched instance property: {property_name} in {target_class.__name__}")

        except Exception as e:
            mesg(f"Failed to patch instance property '{property_name}' in {target_class.__name__}: {e}")
            raise

    def patch_module_property(self, module, property_name, value):
        """Patch module properties to allow observation of gets and sets with exception handling."""
        private_name = f"_{property_name}"
        try:
            setattr(module, private_name, value)
            mesg(f"Patching module property: {property_name} in {module.__name__}")

            def module_getter(instance=None):
                """Modified to accept an optional 'instance' argument for property access."""
                try:
                    current_value = getattr(module, private_name)
                    if rust_mode:
                        return current_value
                    result = self.check_override(value, 'get', module, property_name, current_value, level='module')
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
                    result = self.check_override(value, 'set', module, property_name, new_value, level='module')
                    if isinstance(result, dict):
                        if result.get('do_not_really_set', False): return
                        if 'return_value' in dict:
                            new_value = result.get('return_value')
                    setattr(module, private_name, new_value)
                except Exception as e:
                    mesg(TRACE, f"Error setting module property '{name_dot(module, property_name)}': {e}")
                    raise
    
            setattr(module, property_name, property(fget=module_getter, fset=module_setter))
        except Exception as e:
            mesg(TRACE, f"Failed to patch module property '{name_dot(module, property_name)}': {e}")
            raise

    def patch_module_propertyOLD(self, module, property_name, value):
        """Patch module properties to allow observation of gets and sets with exception handling."""
        private_name = f"_{property_name}"
        try:
            setattr(module, private_name, value)
            mesg(f"Patching module property: {property_name} in {module.__name__}")

            def module_getter():
                try:
                    current_value = getattr(module, private_name)
                    if rust_mode:
                        return current_value
                    result = self.check_override(value, 'get', module, property_name, current_value, level='module')
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
                    result = self.check_override(value, 'set', module, property_name, new_value, level='module')
                    if isinstance(result, dict):
                        if result.get('do_not_really_set', False): return
                        if 'return_value' in dict:
                            new_value = result.get('return_value')
                    setattr(module, private_name, new_value)
                except Exception as e:
                    mesg(TRACE, f"Error setting module property '{name_dot(module, property_name)}': {e}")
                    raise

            setattr(module, property_name, property(fget=module_getter, fset=module_setter))
        except Exception as e:
            mesg(TRACE, f"Failed to patch module property '{name_dot(module, property_name)}': {e}")
            raise


    def patch_instance_method(self, target_class, method_name, method):
        """Patch an instance method to allow observation of calls."""
        original_method = method

        def patched_method(instance_self, *args, **kwargs):
            result = self.check_override(
                original_method, 'before_call', target_class, method_name, args, kwargs, level='instance'
            )
            if isinstance(result, dict) and result.get('do_not_really_call', False):
                return result.get('return_value')
            elif isinstance(result, tuple):
                modified_args, modified_kwargs = result
            else:
                modified_args, modified_kwargs = args, kwargs

            try:
                # Preserve original logic for special methods like __repr__.
                result = original_method(instance_self, *modified_args, **modified_kwargs)
            except Exception as e:
                mesg(f"Error in instance method {method_name}: {e}")
                raise

            modified_result = self.check_override(
                original_method, 'after_call', target_class, method_name, result, level='instance'
            )
            return modified_result if modified_result is not None else result

        # Handle special methods like __repr__ or __str__.
        if method_name.startswith("__") and method_name.endswith("__"):
            mesg(f"Patching special instance method: {name_dot(target_class, method_name)}")
        else:
            mesg(f"Patching instance method: {name_dot(target_class, method_name)}")
    
        setattr(target_class, method_name, patched_method)

    def patch_class_method(self, target_class, method_name, method):
        """Patch a class method to allow observation of calls."""
        # Extract the unbound function from the classmethod object
        original_method = method.__func__

        def patched_class_method(cls, *args, **kwargs):
            result = self.check_override(
                original_method, 'before_call', target_class, method_name, args, kwargs, level='class'
            )
            if isinstance(result, dict) and result.get('do_not_really_call', False):
                return result.get('return_value')
            elif isinstance(result, tuple):
                modified_args, modified_kwargs = result
            else:
                modified_args, modified_kwargs = args, kwargs

            try:
                # Call the unbound original method with 'cls' explicitly
                result = original_method(cls, *modified_args, **modified_kwargs)
            except Exception as e:
                mesg(f"Error in class method {method_name}: {e}")
                raise

            modified_result = self.check_override(
                original_method, 'after_call', target_class, method_name, result, level='class'
            )
            return modified_result if modified_result is not None else result

        setattr(target_class, method_name, classmethod(patched_class_method))
        #mesg(f"Monkey-patched class method {name_dot(target_class, method_name)}")

    def patch_static_method(self, target_class, method_name, method):
        """Patch a static method to allow observation of calls."""
        original_method = method

        def patched_static_method(*args, **kwargs):
            result = self.check_override(
                original_method, 'before_call', target_class, method_name, args, kwargs, level='class'
            )
            if isinstance(result, dict) and result.get('do_not_really_call', False):
                return result.get('return_value')
            elif isinstance(result, tuple):
                modified_args, modified_kwargs = result
            else:
                modified_args, modified_kwargs = args, kwargs

            try:
                result = original_method(*modified_args, **modified_kwargs)
            except Exception as e:
                mesg(f"Error in static method {name_dot(target_class, method_name)}: {e}")
                raise

            modified_result = self.check_override(
                original_method, 'after_call', target_class, method_name, result, level='class'
            )
            return modified_result if modified_result is not None else result

        setattr(target_class, method_name, staticmethod(patched_static_method))
        #mesg(f"Monkey-patched static method {name_dot(target_class, method_name)}")

    def patch_module_function(self, module, function_name, function):
        """Patch a function to allow observation of calls."""
        original_function = function

        def patched_function(*args, **kwargs):
            result = self.check_override(
                original_function, 'before_call', module, function_name, args, kwargs, level='module'
            )
            if isinstance(result, dict) and result.get('do_not_really_call', False):
                return result.get('return_value')
            elif isinstance(result, tuple):
                modified_args, modified_kwargs = result
            else:
                modified_args, modified_kwargs = args, kwargs

            try:
                result = original_function(*modified_args, **modified_kwargs)
            except Exception as e:
                mesg(f"Error in function {name_dot(module, function_name)}: {e}")
                raise

            modified_result = self.check_override(
                original_function, 'after_call', module, function_name, result, level='module'
            )
            return modified_result if modified_result is not None else result

        # Print the signature
        mesg(f"Patching module function: {name_dot(module, function_name)} {signature(original_function)}")
        setattr(module, function_name, patched_function)

    def patch_static_property(self, cls, property_name, value):
        """Patch class (static) properties to allow observation of gets and sets with exception handling."""
        private_name = f"_{property_name}"
        try:
            # Store the original value in a private attribute
            setattr(cls, private_name, value)
            mesg(f"Patching static property: {name_dot(cls,property_name)}")

            # Create getter
            def class_getter():
                try:
                    current_value = getattr(cls, private_name)  # Retrieve the private value
                    #mesg(TRACE, f"Getting static property '{property_name}' with value {current_value}")
                    if rust_mode:
                        return current_value
                    result = self.check_override(value, 'get', cls, property_name, current_value, level="class")
                    if isinstance(result, dict) and result.get('do_not_really_get', False):
                        return result.get('return_value')
                    return current_value
                except Exception as e:
                    mesg(TRACE, f"Error getting static property '{name_dot(cls,property_name)}': {e}")
                    raise

            # Create setter
            def class_setter(new_value):
                try:
                    #mesg(TRACE, f"Setting static property '{property_name}' to {new_value}")
                    if rust_mode:
                        return setattr(cls, private_name, new_value)
                    result = self.check_override(value, 'set', cls, property_name, new_value, level="class")
                    
                    # Handle 'do_not_really_set' and 'really_set' options
                    if isinstance(result, dict):
                        if result.get('do_not_really_set', False): return
                        if 'return_value' in dict:
                            new_value = result.get('return_value')
                        if result.get('really_set', False):
                            new_value = result.get('new_value', new_value)  # Set to the new value if provided

                    setattr(cls, private_name, new_value)
                except Exception as e:
                    mesg(TRACE, f"Error setting static property '{name_dot(cls,property_name)}': {e}")
                    raise

            # Replace the class attribute with a property descriptor
            setattr(cls, property_name, property(fget=class_getter, fset=class_setter))
        except Exception as e:
            mesg(TRACE, f"Failed to patch static property '{name_dot(cls,property_name)}': {e}")
            raise

    def patch_module_variable(self, module, variable_name, value):
        """Patch a module-level variable to allow observation of gets and sets."""
        try:
            self.patch_static_class_property(module, variable_name, value)
            mesg(f"Patching variable: {name_dot(module, variable_name)}")
        except Exception as e:
            mesg(f"Failed to patch module variable {variable_name} in {module.__name__}: {e}")

    def patch_static_class_property(self, cls, property_name, value):
        """Patch a static property to allow observation of gets and sets."""
        try:
            #mesg(f"Patching static property: {name_dot(cls,property_name)}")
            descriptor = ClassVariableDescriptor(property_name, value, self.observer)
            setattr(cls, property_name, descriptor)
            mesg(f"Patching static property: {name_dot(cls,property_name)}")
        except Exception as e:
            mesg(f"Failed to patch static property '{name_dot(cls,property_name)}'  : {e}")
            raise


# Define the ClassVariableDescriptor
class ClassVariableDescriptor:
    def __init__(self, name, initial_value, observer):
        self.name = name
        self.value = initial_value
        self.observer = observer

    def __get__(self, instance, owner):
        current_value = self.value
        result = self.check_override(
            None, 'get', owner, self.name, current_value, level='class'
        )
        if isinstance(result, dict) and result.get('do_not_really_get', False):
            return result.get('return_value')
        return current_value

    def __set__(self, instance, new_value):
        result = self.check_override(
            None, 'set', instance, self.name, new_value, level='class'
        )
        if isinstance(result, dict) and result.get('do_not_really_set', False):
            return
        if isinstance(result, dict) and result.get('really_set', False):
            self.value = result.get('new_value')
            return
        self.value = new_value




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

def classify_member_type(obj, name, member):
    descriptor = get_member_descriptor(obj, name)

    if inspect.isclass(member):
        return MemberType.CLASS
    elif inspect.ismodule(member):
        return MemberType.MODULE
    elif isinstance(descriptor, property):
        return MemberType.PROPERTY
    elif isinstance(descriptor, staticmethod):
        return MemberType.STATIC_METHOD
    elif isinstance(descriptor, classmethod):
        return MemberType.CLASS_METHOD
    elif inspect.isfunction(member):
        if name.startswith('__') and name.endswith('__'):
            return MemberType.SPECIAL_METHOD
        return MemberType.FUNCTION
    elif inspect.ismethod(member):
        return MemberType.METHOD
    elif not callable(member):
        if name.startswith('__') and name.endswith('__'):
            return MemberType.SPECIAL_VARIABLE
        return MemberType.VARIABLE
    else:
        if name.startswith('__') and name.endswith('__'):
            return MemberType.SPECIAL_METHOD
        return MemberType.METHOD

def get_member_descriptor(obj, name):
    if inspect.isclass(obj):
        if name in obj.__dict__:
            return obj.__dict__[name]
        for base in inspect.getmro(obj)[1:]:
            if name in base.__dict__:
                return base.__dict__[name]
    elif inspect.ismodule(obj):
        if name in obj.__dict__:
            return obj.__dict__[name]
    return None

def classify_member_level(obj, name, member, implemented_from, uses_self_flag):
    descriptor = get_member_descriptor(obj, name)

    if inspect.isclass(obj):
        if isinstance(descriptor, property):
            return MemberLevel.INSTANCE
        elif isinstance(descriptor, staticmethod):
            return MemberLevel.CLASS
        elif isinstance(descriptor, classmethod):
            return MemberLevel.CLASS
        elif not callable(member):
            return MemberLevel.CLASS
        elif uses_self_flag:
            return MemberLevel.INSTANCE
        elif implemented_from == 'object':
            return MemberLevel.MODULE
        else:
            return MemberLevel.CLASS
    elif inspect.ismodule(obj):
        return MemberLevel.MODULE
    else:
        return MemberLevel.CLASS

def classify_implementation(obj, implemented_from):
    if implemented_from == obj.__name__:
        return Implemented.LOCAL.value
    elif implemented_from == 'object':
        return Implemented.DEFAULT.value
    elif implemented_from is not None:
        return Implemented.INHERITED.value
    else:
        return Implemented.LOCAL.value



#!/usr/bin/env python3
# Filename: test_metta_python_patcher.py
# Ensure this part is consistent (we keep the contents below maintained and ready to be put into our own file)
from metta_python_patcher import Observer, mesg, name_dot
import hyperonpy
from hyperon.runner import MeTTa
from hyperon.atoms import GroundedAtom, VariableAtom, SymbolAtom, ExpressionAtom, ValueObject, OperationObject, MatchableObject

# Flag to switch between using simple `if` or `assert`
USE_ASSERT = False

# ANSI color code for cyan
CYAN = "\033[96m"
RESET = "\033[0m"

def commentary(message):
    """Function to print a message in cyan for debug commentary."""
    print(f"{CYAN}{message}{RESET}")

def check_equal(expected, actual, message=""):
    """Check if two values are equal, either using assert or if."""
    # ANSI color codes for red and green
    RED = "\033[91m"
    GREEN = "\033[92m"
    
    commentary(f"DEBUG: {message} - Expected: {expected}, Actual: {actual}")
    if USE_ASSERT:
        assert expected == actual, f"{message} Expected: {expected}, got: {actual}"
    else:
        if expected != actual:
            print(f"{RED}ERROR: {message} Expected: {expected}, got: {actual}{RESET}")
        else:
            print(f"{GREEN}SUCCESS: {message} Expected and got: {expected}{RESET}")


# Example classes for testing
class BaseClass:
    base_class_var = 41  # Class-level attribute

    def __init__(self):
        self.base_instance_var = 42  # Base class instance variable

    def base_instance_method(self):
        return self.base_instance_var

    def __str__(self):
        return "base_str"

    def __repr__(self):
        return "base_repr"

    def base_method(self, value):
        return value * 2

    @classmethod
    def base_class_method_without_self(cls):
        pass

    @classmethod
    def base_class_method_with_self(cls, self):
        pass

    @property
    def base_property(self):
        return self.base_instance_var


class MyClass(BaseClass):
    class_var = 100  # Class-level attribute
    class_value = 100  # Static property
    other_class_value = 666  # Another static property

    def __init__(self, value="inst_value"):
        super().__init__()  # Call base class __init__ to inherit base instance variables
        self.value = value
        self.instance_var = 10  # Instance-level attribute

    def __repr__(self):
        return repr(self.value)

    def my_method(self, x):
        return x * 2

    @staticmethod
    def static_method(y=1999):
        return y + 100

    @classmethod
    def class_method(cls, z):
        return z * 3

    @classmethod
    def class_method_without_self(cls):
        pass

    @classmethod
    def class_method_with_self(cls, self):
        pass

    def instance_method(self):
        return self.instance_var

    @property
    def my_instance_property(self):
        return self.instance_var

    @my_instance_property.setter
    def my_instance_property(self, value):
        self.instance_var = value


class TestClassV2:
    def __init__(self, value="inst_value"):
        self.instance_var = 10  # Initialize instance_var

    @property
    def test_property(self):
        # Getter should return instance_var
        return self.instance_var

    @test_property.setter
    def test_property(self, value):
        # Setter should update instance_var
        print(f"Setting test_property: {value}")
        self.instance_var = value
        print(f"instance_var is now: {self.instance_var}")





# Test case for demoHyperonPy
def test_demoHyperonPy(): 
    commentary("Starting test_demoHyperonPy()... saving CLOSE_PAREN before we do any overrides") 
    
    # Check if hyperonpy.CLOSE_PAREN is a property
    commentary("Checking if hyperonpy.CLOSE_PAREN is a property")
    const = hyperonpy.CLOSE_PAREN
    if isinstance(const, property):
        commentary("CLOSE_PAREN is a property. Accessing the actual value of the property.")
        const = const.fget(hyperonpy)  # Access the property value if it's a property
    
    commentary("Creating a fresh overrider system for HyperonPy")
    overrider = Overrider()  # Ensure everything is set up

    commentary("Inspecting the hyperonpy module")
    overrider.monkey_patch_module(hyperonpy)

    commentary("Patching all members of hyperonpy module")
    overrider.monkey_patch_members()

    commentary("Creating a MeTTa instance for further testing")
    metta = MeTTa()

    commentary("Running a simple MeTTa expression '!(+ 1 1)'")
    result = metta.run("!(+ 1 1)")

    commentary("Checking if MeTTa run result matches the expected output")
    check_equal("[[2]]", repr(result), "MeTTa run result")  # Expected output from MeTTa

    commentary("Ensuring hyperonpy.CLOSE_PAREN is consistent")
    # Retrieve CLOSE_PAREN again after monkey-patching
    close_paren_after_patch = hyperonpy.CLOSE_PAREN
    if isinstance(close_paren_after_patch, property):
        commentary("CLOSE_PAREN is still a property after patching. Accessing its actual value.")
        close_paren_after_patch = close_paren_after_patch.fget(hyperonpy)

    check_equal(const, close_paren_after_patch, "hyperonpy.CLOSE_PAREN")

    commentary("Finished with test_demoHyperonPy\n\n")

def demoAtoms():
    commentary("Starting demoAtoms() by setting up the overrider system")
    overrider = Overrider()  # Ensure everything is set up

    # First, run the tests without monkey-patching
    commentary("Running the tests without any monkey-patching to check base behavior")

    # Create a ValueObject instance without any monkey-patches
    atom_no_patch = ValueObject("I am a String")
    
    commentary("Accessing 'value' property of ValueObject (no monkey patch), expecting 'I am a String'")
    check_equal("I am a String", atom_no_patch.value, "ValueObject value (no patch)")

    commentary("Accessing 'str' representation of ValueObject (no monkey patch), expecting '\"I am a String\"'")
    check_equal('"I am a String"', str(atom_no_patch), "ValueObject str (no patch)")

    commentary("Accessing 'repr' representation of ValueObject (no monkey patch), expecting '\"I am a String\"'")
    check_equal('"I am a String"', repr(atom_no_patch), "ValueObject repr (no patch)")

    # Now, proceed with monkey-patching and repeat the test
    commentary("Marking and inspecting ValueObject and other atom-related classes for monkey-patching")
    overrider.mark_class(GroundedAtom)
    overrider.mark_class(VariableAtom)
    overrider.mark_class(SymbolAtom)
    overrider.mark_class(ExpressionAtom)
    overrider.mark_class(ValueObject)
    overrider.mark_class(OperationObject)
    overrider.mark_class(MatchableObject)

    commentary("Marking base classes")
    overrider.mark_base_classes(also_static=False)

    commentary("Monkey-patching captured members")
    overrider.monkey_patch_members()

    commentary("Creating a ValueObject instance (with monkey-patching) with the value 'I am a String'")
    atom = ValueObject("I am a String")

    commentary("Accessing the 'value' property of ValueObject (with patch), expecting 'I am a String'")
    check_equal("I am a String", atom.value, "ValueObject value (with patch)")

    commentary("Accessing 'str' representation of ValueObject (with patch), expecting '\"I am a String\"'")
    check_equal('"I am a String"', str(atom), "ValueObject str (with patch)")

    commentary("Accessing 'repr' representation of ValueObject (with patch), expecting '\"I am a String\"'")
    check_equal('"I am a String"', repr(atom), "ValueObject repr (with patch)")

    commentary("Finished with demoAtoms\n\n")


# Test case for MyClass
def demoMyClass():
    commentary("Creating a fresh overrider system for demoMyClass()")
    overrider = Overrider()  # Ensure everything is set up

    commentary("Marking MyClass for inspection and filtering out any irrelevant base classes")
    overrider.mark_class(MyClass)
    overrider.mark_base_classes()

    commentary("Attempting to patch all members of MyClass for observation and overrides")
    overrider.monkey_patch_members()

    # Test the monkey-patched methods
    commentary("Creating an instance of MyClass. Expecting instance_var to be initialized to 10 and value to 'inst_value'")
    obj = MyClass()
    
    commentary("Accessing the 'my_instance_property' property of the MyClass instance, expecting it to return 10 (initial value)")
    check_equal(10, obj.my_instance_property, "MyClass initial my_instance_property")
    
    commentary("Setting 'my_instance_property' to 200. This will modify the internal 'instance_var' of MyClass")
    obj.my_instance_property = 200
    
    commentary("Re-accessing 'my_instance_property' after modification, expecting it to return 200")
    check_equal(200, obj.my_instance_property, "MyClass updated my_instance_property")

    commentary("Calling 'instance_method' of MyClass, expecting it to return 'instance_var' (which should now be 200)")
    check_equal(200, obj.instance_method(), "MyClass instance_method result")

    commentary("Getting the string representation of MyClass using 'repr', expecting it to return the 'value' attribute")
    repr_value = repr(obj)
    check_equal("'inst_value'", repr_value, "MyClass repr")

    commentary("Calling 'class_method_without_self', a class method that does not require an instance")
    MyClass.class_method_without_self()

    commentary("Calling 'class_method_with_self' with the current instance of MyClass")
    MyClass.class_method_with_self(obj)

    commentary("Calling 'static_method' with default argument, expecting it to return 2099 (1999 + 100)")
    check_equal(2099, MyClass.static_method(), "MyClass static_method")

    commentary("Setting 'class_var' (class-level variable) to 300 and checking if it's updated correctly")
    MyClass.class_var = 300
    check_equal(300, MyClass.class_var, "MyClass class_var")

    commentary("Finished with demoMyClass\n\n")


# Test case for TestClassV2
def test_TestClassV2():
    commentary("Setting up a fresh overrider system for TestClassV2")
    overrider = Overrider()

    commentary("Marking TestClassV2 for inspection")
    overrider.mark_class(TestClassV2)

    commentary("Attempting to patch all members of TestClassV2")
    overrider.monkey_patch_members()

    commentary("Creating an instance of TestClassV2. The constructor should initialize 'instance_var' to 10")
    obj = TestClassV2()

    commentary("Accessing 'test_property' of the TestClassV2 instance, expecting it to return 10 (initial value of 'instance_var')")
    check_equal(10, obj.test_property, "TestClassV2 initial test_property")

    commentary("Setting 'test_property' to 200, which updates 'instance_var' to 200")
    obj.test_property = 200

    commentary("Re-accessing 'test_property' after modification, expecting it to return 200")
    check_equal(200, obj.test_property, "TestClassV2 updated test_property")

    commentary("Setting 'test_property' again to 500, this should update 'instance_var' to 500")
    obj.test_property = 500

    commentary("Verifying that 'test_property' now returns 500 after the second update")
    check_equal(500, obj.test_property, "TestClassV2 second update test_property")

    commentary("Finished with test_TestClassV2\n\n")

# Example class for testing method override behavior
class OverrideClass:
    def __init__(self):
        self.instance_var = 10

    def multiply(self, value):
        return value * 2

    @property
    def dynamic_property(self):
        return self.instance_var

    @dynamic_property.setter
    def dynamic_property(self, value):
        self.instance_var = value
        
    @staticmethod
    def static_method():
        return 555

        

# Test case for testing method override in OverrideClass
def test_method_override():
    commentary("Setting up a fresh overrider system for method override testing in OverrideClass")
    overrider = Overrider(False)  # Ensure everything is set up

    commentary("Subscribing to 'before_call' to override 'multiply' method when called with argument 5")
    def before_multiply_override(*args):
        target_class, method_name = args[0], args[1]
        method_args = args[2]  # Unpack the method's actual arguments here
        mesg(f"[Override] Before calling {name_dot(target_class, method_name)} with args {method_args}")
        
        # Check if we are calling 'multiply' and if the argument passed is 5
        if method_name == "multiply" and len(method_args) > 0 and method_args[0] == 5:
            commentary(f"Overriding {name_dot(target_class, method_name)} to return 100 when input is 5")
            return {'do_not_really_call': True, 'return_value': 100}  # Override return value if argument is 5

    overrider.add_override('before_call', before_multiply_override, level='instance')

    commentary("Marking OverrideClass for inspection")
    overrider.mark_class(OverrideClass)

    commentary("Attempting to patch all members of OverrideClass")
    overrider.monkey_patch_members()

    commentary("Creating an instance of OverrideClass, which initializes 'instance_var' to 10")
    obj = OverrideClass()

    commentary("Calling 'multiply' with 3, expecting it to return 6 (standard behavior: 3 * 2)")
    check_equal(6, obj.multiply(3), "OverrideClass multiply(3)")

    commentary("Calling 'multiply' with 5, expecting it to return 100 (overridden behavior)")
    check_equal(100, obj.multiply(5), "OverrideClass multiply(5) override")

    commentary("Finished with test_method_override\n\n")


# Test case for testing property getter override in OverrideClass
def test_property_get_override():
    commentary("Setting up a fresh overrider system for property getter override in OverrideClass")
    overrider = Overrider(False)  # Set up the overrider and patcher

    commentary("Subscribing to 'get' to override 'dynamic_property' to always return 999")

    # Define the override for the 'get' event for dynamic_property
    def override_dynamic_property_get(*args):
    
    
        target_class, property_name = args[0], args[1]
        #current_value = args[2]  # Fetch the current value if needed
        #mesg(f"[Override] Overriding {name_dot(target_class, property_name)} getter")
    
        if property_name == "dynamic_property":
            # Return the overridden value for dynamic_property
            return {'do_not_really_get': True, 'return_value': 999}
    
        return {}


    overrider.add_override('get', override_dynamic_property_get, level='instance')

    commentary("Marking OverrideClass for inspection")
    overrider.mark_class(OverrideClass)

    commentary("Attempting to patch all members of OverrideClass")
    overrider.monkey_patch_members()

    commentary("Creating an instance of OverrideClass, which initializes 'instance_var' to 10")
    obj = OverrideClass()

    commentary("Accessing 'dynamic_property' expecting the overridden value of 999 (instead of 10)")

    # Adding explicit check if dynamic_property was patched
    if not hasattr(obj.__class__, 'dynamic_property'):
        mesg("[Error] dynamic_property not found or not patched correctly.")
    
    value = obj.dynamic_property
    mesg(f"Retrieved value of dynamic_property: {value}")
    check_equal(999, value, "OverrideClass dynamic_property override")

    commentary("Finished with test_property_get_override\n\n")


# Test case for testing property setter override in OverrideClass
def test_property_set_override():
    commentary("Setting up a fresh overrider system for property setter override in OverrideClass")
    overrider = Overrider(False)  # Ensure everything is set up

    commentary("Subscribing to 'set' to block setting 'dynamic_property' to 300")
    def set_property_override(*args):
        target_class, property_name, new_value = args[0], args[1], args[2]
        mesg(f"[Override] Setting {name_dot(target_class, property_name)} to {new_value}")
        if property_name == "dynamic_property" and new_value == 300:
            commentary(f"Blocking {name_dot(target_class, property_name)} from being set to 300")
            return {'do_not_really_set': True}  # Block setting if the new value is 300

    overrider.add_override('set', set_property_override, level='instance')

    commentary("Marking OverrideClass for inspection")
    overrider.mark_class(OverrideClass)

    commentary("Attempting to patch all members of OverrideClass")
    overrider.monkey_patch_members()

    commentary("Creating an instance of OverrideClass, which initializes 'instance_var' to 10")
    obj = OverrideClass()

    commentary("Setting 'dynamic_property' to 200 (expected to succeed) and verifying")
    obj.dynamic_property = 200
    check_equal(200, obj.dynamic_property, "OverrideClass dynamic_property set to 200")

    commentary("Attempting to set 'dynamic_property' to 300 (expected to be blocked)")
    obj.dynamic_property = 300
    check_equal(200, obj.dynamic_property, "OverrideClass dynamic_property set to 300 blocked")

    commentary("Finished with test_property_set_override\n\n")


# Test case for testing static method override in OverrideClass
def test_static_method_override():
    commentary("Setting up a fresh overrider system for static method override in OverrideClass")
    overrider = Overrider()  # Ensure everything is set up

    commentary("Subscribing to 'before_call' to override 'static_method' to return 777")
    def before_static_method_override(*args):
        target_class, method_name = args[0], args[1]
        mesg(f"[Override] Before calling {name_dot(target_class, method_name)}")
        if method_name == "static_method":
            commentary(f"Overriding {name_dot(target_class, method_name)} to return 777")
            return {'do_not_really_call': True, 'return_value': 777}  # Override static method return value

    overrider.add_override('before_call', before_static_method_override, level='class')

    commentary("Marking OverrideClass for inspection")
    overrider.mark_class(OverrideClass, also_static=True)

    commentary("Attempting to patch all members of OverrideClass")
    overrider.monkey_patch_members()

    commentary("Calling 'static_method', expecting the overridden return value of 777")
    check_equal(777, OverrideClass.static_method(), "OverrideClass static_method override")

    commentary("Finished with test_static_method_override\n\n")

# Example base and derived classes for testing overrides
class BaseClassForOverride:
    def __init__(self):
        self.instance_var = 10

    def base_method(self, value):
        return value * 2

    def pass_method(self, value):
        return value * 2

    def override_base_method(self, value):
        return value * 2


class DerivedClassForOverride(BaseClassForOverride):
    def derived_method(self, value):
        return self.base_method(value) + 1  # Calls the base method and adds 1

    def pass_method(self, value):
        pass

    def override_base_method(self, value):
        return value * 3

# Test case for testing base method override in derived class
def test_base_method_override():
    commentary("Setting up a fresh overrider system for base method override in DerivedClassForOverride")
    overrider = Overrider()

    commentary("Subscribing to 'before_call' to override 'base_method' to return 100 if called with 5")
    def before_base_method_override(*args):
        target_class, method_name = args[0], args[1]
        mesg(f"[Override] Before calling {name_dot(target_class, method_name)}")
        if method_name == "base_method" and args[2] == (5, ):
            commentary(f"Overriding {name_dot(target_class, method_name)} to return 100 when input is 5")
            return {'do_not_really_call': True, 'return_value': 100}  # Override return value if argument is 5

    overrider.add_override('before_call', before_base_method_override, level='instance')

    commentary("Marking BaseClassForOverride and DerivedClassForOverride for inspection")
    overrider.mark_class(BaseClassForOverride)
    overrider.mark_class(DerivedClassForOverride)

    commentary("Attempting to patch all members of both classes")
    overrider.monkey_patch_members()

    commentary("Creating an instance of DerivedClassForOverride (inheriting from BaseClassForOverride)")
    obj = DerivedClassForOverride()

    commentary("Calling 'derived_method' with 3, expecting it to call 'base_method' and return 7 (3 * 2 + 1)")
    check_equal(7, obj.derived_method(3), "DerivedClassForOverride derived_method(3)")

    commentary("Calling 'derived_method' with 5, expecting overridden behavior to return 101")
    check_equal(101, obj.derived_method(5), "DerivedClassForOverride derived_method(5) override")

    commentary("Finished with test_base_method_override\n\n")


# Main function to execute all tests
def main():
    commentary("Starting the main test suite")
    #test_demoHyperonPy()  # Test the HyperonPy and MeTTa patching
    #demoAtoms()  # Inspect and patch Atom classes
    demoMyClass()  # Inspect and patch MyClass
    test_TestClassV2()  # Test property access and modification for TestClassV2
    test_method_override()  # Test method overriding
    test_property_get_override()  # Test property getter override
    test_property_set_override()  # Test property setter override
    test_static_method_override()  # Test static method override
    test_base_method_override()  # Test base method overriding
    commentary("Finished executing the main test suite")


# Execute main if run as a script
if __name__ == "__main__":
    main()

# This file is ran in bash as:
# clear ; python src/canary/metta_python_patcher.py
# chatgpt will examine the failures and try to correct the code
