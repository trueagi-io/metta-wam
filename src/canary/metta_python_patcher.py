#!/usr/bin/env python3
# Filename: metta_python_patcher.py
import inspect
import os
from enum import Enum, auto
import sys
import types
import threading  # For thread-local storage if needed
import atexit
import traceback

# Module-level verbosity levels for logging
SILENT = 0  # No output
USER = 1    # Basic output for users
DEBUG = 2   # Detailed debug information
TRACE = 3   # Granular trace-level output
DEVEL = DEBUG
# Default verbosity level
METTALOG_VERBOSE = USER

# Global variables (placeholders for potential use)
global runner
runner = None
global norepl_mode
norepl_mode = True
global catom_mode
catom_mode = False
global cb
cb = None
global rust_mode
rust_mode = False  # Set to False as default

# ANSI color codes for terminal output (used in commentary)
CYAN = "\033[96m"
RESET = "\033[0m"

def commentary(message):
    """Function to print a message in cyan for debug commentary."""
    print(f"{CYAN}{message}{RESET}")

# Log messages based on verbosity level
def mesg(level, message='', m2=None, m3=None):
    """Logging function that outputs messages based on the METTALOG_VERBOSE level."""
    if not isinstance(level, int):
        message = f"{level} {message}"
        level = TRACE

    message = f"{message} {m2 or ''} {m3 or ''}"

    # For simplicity, we print all messages. In production, you might check METTALOG_VERBOSE.
    print(message, RESET)
    return
    if METTALOG_VERBOSE >= level:
        print(message)

def set_no_mock_objects_flag(flag: bool):
    """Function to set a flag for mocking behavior (not used in this code)."""
    global mock_throw_error
    mock_throw_error = flag

def get_no_mock_objects_flag() -> bool:
    """Function to get the mocking behavior flag (not used in this code)."""
    return mock_throw_error

# Set verbosity level
def set_verbosity(level):
    """Set the verbosity level for logging."""
    global METTALOG_VERBOSE
    newlevel = int(level)
    if isinstance(newlevel, int):
        METTALOG_VERBOSE = newlevel
        mesg(DEBUG, f"Verbosity set to level {newlevel}")
    else:
        print(f"Invalid verbosity level '{level}' provided. Keeping level: {METTALOG_VERBOSE}.")

# Initialize verbosity from environment variable or command-line arguments
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
    """Helper function to create a qualified name."""
    if module is None:
        return name
    try:
        return f"{module.__name__}.{name}"
    except Exception as _:
        return f"{module}.{name}"


def signature(obj):
    """Attempt to get the signature of a callable object."""
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
    """Placeholder function to ignore exceptions."""
    pass

# Define Enums for member types, levels, and implementation status
class MemberType(Enum):
    """Enumeration for different types of members."""
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
    """Enumeration for the level at which a member exists."""
    INSTANCE = "instance"  # For instance methods and properties
    CLASS = "class"        # For class methods, static methods, and class variables
    MODULE = "module"      # For module-level members

class Implemented(Enum):
    """Enumeration for the implementation status of a member."""
    LOCAL = "Local"          # Implemented in the current class/module
    INHERITED = "Inherited"  # Inherited from a base class
    DEFAULT = "Default"      # Inherited from 'object'

# Observer Class
class Observer:
    """Observer class to manage event subscriptions and notifications."""

    def __init__(self):
        self.subscribers = {}

    def subscribe(self, event_type, callback, level="instance"):
        """Override to an event type ('before_call', 'after_call', 'set', 'get') with a callback."""
        key = f"{level}_{event_type}"
        if key not in self.subscribers:
            self.subscribers[key] = []
        self.subscribers[key].append(callback)

    def notify(self, member_info, event_type, *args, **kwargs):
        """Notify all overriders for a specific event type."""
        # Use method-local suspend_trace
        suspend_trace = False
        level = kwargs.pop('level', 'instance')  # Remove 'level' from kwargs
        args_list = list(args)

        # Skip certain substrings in the name (adjust or remove based on your needs)
        if False:
            if len(args_list) > 1:
                for substring in ["tokenizer", "metta_err_str", "_free"]:
                    if substring in str(args_list[1]):
                        return

        # Generate the event key based on the level and event type
        event_key = f"{level}_{event_type}"
        final_result = None  # Store the first valid result here

        # Check if there are any subscribers overridden to this event key
        if event_key in self.subscribers:
            for callback in self.subscribers[event_key]:
                result = callback(*args)
                # Apply the first valid override (with 'overridden_value' or 'do_not_really_*')
                if result and ('return_value' in result or 'do_not_really_*' in result):
                    # If we don't have a final result yet, accept this one
                    if final_result is None:
                        final_result = result
                # Notify others even if we already have a valid result

        # Return the first valid result, or None if no override was found
        return final_result or kwargs.get('default_value')


class PropertyStorage(Enum):
    DICT = "dict"
    ATTRIBUTE = "attribute"
    ORIGINAL_PROPERTY = "original_property"
    DESCRIPTOR = "descriptor"


class OverrideResult(Enum):
    NO_OVERRIDE = auto()  # No override occurred (subscriber is just observing)
    NOT_FOUND = auto()  # Property not found (perhaps return TypeError)
    NOT_SET = auto()  # Property should not be set (skip setting the original)
    RESULT_NONE = auto()  # Property value equals None


# For convenience, alias the enum values to match your current placeholders
NO_OVERRIDE = OverrideResult.NO_OVERRIDE
NOT_FOUND = OverrideResult.NOT_FOUND
NOT_SET = OverrideResult.NOT_SET
RESULT_NONE = OverrideResult.RESULT_NONE


def patch_property(overrider, target, property_name, original_property, level, member_info):
    """Patch an instance, class, or module property or descriptor to allow observation of gets and sets."""
    private_name = f"_{property_name}"
    suspend_trace = False  # Initialize the 'suspend_trace' flag for this property
    target_type = target if level in ["class", "module"] else type(target)

    # Track whether we are using private_name for this property
    using_private_name = False

    # Initialize a basic better description for logging
    better_description = f"{level.capitalize()} level property '{name_dot(target, property_name)}' in {target_type.__name__ if hasattr(target_type, '__name__') else target_type}"

    mesg(f"Patching {level} property: {better_description}")

    # Determine where the property is stored initially
    property_location = None
    is_readonly = False  # Flag to track if the property is read-only

    def determine_property_location():
        nonlocal property_location, is_readonly, using_private_name, better_description

        # Try to determine where the property is stored
        if original_property is None:
            # No original property provided, default to using __dict__
            property_location = PropertyStorage.DICT
        elif isinstance(original_property, property):
            property_location = PropertyStorage.ORIGINAL_PROPERTY
            is_readonly = original_property.fset is None  # Check if the property is readonly
        elif hasattr(original_property, '__get__') or hasattr(original_property, '__set__'):
            property_location = PropertyStorage.DESCRIPTOR
            is_readonly = not hasattr(original_property, '__set__')  # Check if custom descriptor is readonly
        elif level == "module" and hasattr(target, "__dict__") and property_name in target.__dict__:
            property_location = PropertyStorage.DICT
        elif level == "class" and hasattr(target_type, "__dict__") and property_name in target_type.__dict__:
            property_location = PropertyStorage.DICT
        elif hasattr(target, "__dict__") and property_name in target.__dict__:
            property_location = PropertyStorage.DICT
        else:
            property_location = PropertyStorage.ATTRIBUTE  # Fallback to attribute if not found in dict

        # Check if we are using private_name to store values
        if property_location == PropertyStorage.DICT and private_name in (
                target.__dict__ if hasattr(target, '__dict__') else {}):
            using_private_name = True

        # Append the storage method and readonly status to better_description
        location_desc = property_location.value
        readonly_desc = "readonly" if is_readonly else "writable"
        better_description += f" ({readonly_desc}, {location_desc} storage)"

        mesg(f"Determined property location for '{better_description}'")

        
    def patched_getter(instance=None):
        nonlocal suspend_trace
        current_value = NOT_FOUND  # Initialize the current value to NOT_FOUND
        result_before = NOT_FOUND  # Start with NO_OVERRIDE
    
        # Determine property location on the first access
        if property_location is None:
            determine_property_location()
    
        if not (rust_mode or suspend_trace):
            suspend_trace = True
            try:
                # Before Get Event
                result_before = overrider.check_override(
                    member_info, 'before_get', instance or target, property_name, level=level
                )
                # If there's a valid override, return it, but not if it's NO_OVERRIDE
                if result_before is not NO_OVERRIDE:
                    return result_before
            except Exception as e:
                mesg(f"Error in 'before_get' override for {better_description}: {e}")
                raise
            finally:
                suspend_trace = False
    
        try:
            # Fetch the value based on the determined location
            if property_location == PropertyStorage.ORIGINAL_PROPERTY and original_property is not None:
                # Use the original property getter directly
                if original_property.fget:
                    current_value = original_property.fget(instance or target)
            elif property_location == PropertyStorage.DESCRIPTOR and original_property is not None:
                # Use the original descriptor's getter
                current_value = original_property.__get__(instance or target, target_type)
            elif property_location == PropertyStorage.DICT:
                # Use private_name if it's tracked for this property
                if using_private_name:
                    current_value = getattr(instance or target, private_name, NOT_FOUND)
                else:
                    current_value = getattr(instance or target, property_name, NOT_FOUND)
            elif property_location == PropertyStorage.ATTRIBUTE:
                current_value = getattr(instance or target, property_name, NOT_FOUND)
    
            if current_value is NOT_FOUND:
                raise TypeError(f"{property_name} not found on {target_type}")
    
            # After Get Event
            suspend_trace = True
            try:
                result_after = overrider.check_override(
                    member_info, 'after_get', instance or target, property_name, current_value, level=level
                )
                # If after_get event is overridden, return that value, but not NO_OVERRIDE
                if result_after is not NO_OVERRIDE:
                    return result_after
            except Exception as e:
                mesg(f"Error in 'after_get' override for {better_description}: {e}")
                raise
            finally:
                suspend_trace = False
    
        except Exception as e:
            mesg(f"Error getting {better_description}: {e}")
            raise
    
        # Ensure we return the correct property value (current_value) at the end
        return current_value
    
    
    def patched_setter(instance, new_value):
        nonlocal suspend_trace
        result_before = NO_OVERRIDE  # Initialize result_before to NO_OVERRIDE
    
        # Determine property location on the first access
        if property_location is None:
            determine_property_location()
    
        if not (rust_mode or suspend_trace):
            suspend_trace = True
            try:
                mesg(f"Calling patched_setter for '{better_description}' with value {new_value}")
                # Before Set Event
                result_before = overrider.check_override(
                    member_info, 'before_set', instance or target, property_name, new_value, level=level
                )
                # If the result_before is NOT_SET or NO_OVERRIDE, don't set it
                if result_before is NOT_SET:
                    return  # Skip setting the value
                if result_before is not NO_OVERRIDE:
                    new_value = result_before  # Only change new_value if it's not NO_OVERRIDE
            except Exception as e:
                mesg(f"Error in 'before_set' override for {better_description}: {e}")
                raise
            finally:
                suspend_trace = False
    
        # If the property is readonly, prevent setting unless before_set allows it
        if is_readonly and result_before is NO_OVERRIDE:
            raise AttributeError(f"Can't set {better_description} because it's read-only")
    
        try:
            # Set the value based on the determined location
            if property_location == PropertyStorage.ORIGINAL_PROPERTY and original_property is not None and original_property.fset:
                # Use the original property setter directly
                original_property.fset(instance or target, new_value)
            elif property_location == PropertyStorage.DESCRIPTOR and hasattr(original_property, '__set__'):
                # Use the original descriptor's setter
                original_property.__set__(instance or target, new_value)
            elif property_location == PropertyStorage.DICT:
                # Use private_name if it's tracked for this property
                if using_private_name:
                    setattr(instance or target, private_name, new_value)
                else:
                    setattr(instance or target, property_name, new_value)
            elif property_location == PropertyStorage.ATTRIBUTE:
                setattr(instance or target, property_name, new_value)
    
            # After Set Event
            suspend_trace = True
            try:
                result_after_set = overrider.check_override(
                    member_info, 'after_set', instance or target, property_name, new_value, level=level
                )
                # Do not use NO_OVERRIDE as the final set value
                if result_after_set is NO_OVERRIDE:
                    result_after_set = None  # Reset to None if it's NO_OVERRIDE
            except Exception as e:
                mesg(f"Error in 'after_set' override for {better_description}: {e}")
                raise
            finally:
                suspend_trace = False
    
        except Exception as e:
            mesg(f"Error setting {better_description}: {e}")
            raise

    try:
        # Handle case where original_property is None or it's readonly
        if original_property is None:
            mesg(f"No original property provided for {better_description}. Generating default descriptor.")
            new_property = property(fget=patched_getter, fset=patched_setter)  # Create a default property
        elif isinstance(original_property, property):
            # Apply as a property, handle readonly case
            if is_readonly:
                new_property = property(fget=patched_getter)  # Readonly property with no setter
            else:
                new_property = property(fget=patched_getter, fset=patched_setter)
        elif isinstance(original_property, staticmethod):
            # Apply as a static method
            new_property = staticmethod(patched_getter)
        elif isinstance(original_property, classmethod):
            # Apply as a class method
            new_property = classmethod(patched_getter)
        elif hasattr(original_property, '__get__') and hasattr(original_property, '__set__'):
            # Apply as a custom descriptor
            new_property = type(original_property)(patched_getter, patched_setter)
        else:
            # Apply as a property by default if no specific type is identified
            new_property = property(fget=patched_getter, fset=patched_setter)

        # Store the original method and set the patched version
        overrider.original_methods[(target, property_name)] = original_property  # Store original
        setattr(target, property_name, new_property)

        mesg(f"Successfully patched {better_description}")

    except Exception as e:
        mesg(f"Failed to patch {better_description}: {e}")
        raise


# Monkey Patching Class
class Overrider:
    """Classtomonkey-patchmethodsandproperties,andmanageoverrides."""

    def __init__(self, all=True):
        self.observer = Observer()
        self.attempted_to_capture_init_variables_on_classes = set()
        self.marked_classes = set()
        self.marked_members = set()
        self.class_hierarchy = {}
        self.marked_member_info_by_classname = {}
        self.patched_members = {}
        self.original_methods = {}  # Dictionary to store original methods
        self.set_default_observers(all, all)

    # Setup observer and monkey-patcher system
    def set_default_observers(self, allDefaults, also_static):
        """Set default observers for various events."""

        # Default callbacks for events (for demonstration purposes)
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

    def check_override(self, member_info, event_type, *args, **kwargs):
        return self.observer.notify(member_info, event_type, *args, **kwargs)

    def add_override(self, event_type, callback, level="instance"):
        return self.observer.subscribe(event_type, callback, level)

    def mark_members(self, obj, also_static=False):
        """Mark members of a class or module for monkey-patching."""
        if obj is not None:
            if inspect.isclass(obj):
                self.mark_class(obj, also_static)
            elif inspect.ismodule(obj):
                self.monkey_patch_module(obj)
            else:
                self.mark_class(type(obj))

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

            # Skip special variables
            if member_level == MemberLevel.CLASS and member_type == MemberType.SPECIAL_VARIABLE:
                continue

            # Skip static members if not requested
            if member_level == MemberLevel.CLASS and not also_static:
                continue

            # Skip inherited members if not requested
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
        member = member_info['member']
        level = member_info['level']
        member_type = member_info['member_type']

        qualified_name = f"{name_dot(cls, name)}"
        if callable(member):
            qualified_name = f"{qualified_name}{signature(member)}"

        if member in self.patched_members:
            return  # Skip if already patched

        self.patched_members[member] = member_info

        try:
            if member_type == MemberType.METHOD:
                if level == MemberLevel.INSTANCE:
                    mesg(f"Patching instance method: {qualified_name}")
                    self.patch_instance_method(cls, name, member, member_info)
                elif level == MemberLevel.CLASS:
                    mesg(f"Patching static class method: {qualified_name}")
                    self.patch_class_method(cls, name, member, member_info)
                else:
                    mesg(f"Skipping method {qualified_name} with unknown level: {level}")
            elif member_type == MemberType.CLASS_METHOD:
                mesg(f"Patching (static) class method: {qualified_name}")
                self.patch_class_method(cls, name, member, member_info)
            elif member_type == MemberType.STATIC_METHOD:
                mesg(f"Patching static method: {qualified_name}")
                self.patch_static_method(cls, name, member, member_info)
            elif member_type == MemberType.PROPERTY:
                mesg(f"Patching property: {qualified_name}")
                # Retrieve the original property descriptor
                original_property = get_member_descriptor(cls, name)
                self.patch_instance_property(cls, name, original_property, member_info, level='instance')
            elif member_type == MemberType.VARIABLE:
                if level == MemberLevel.CLASS:
                    mesg(f"Patching static class variable: {qualified_name}")
                    self.patch_static_property(cls, name, member, member_info)
                elif level == MemberLevel.INSTANCE:
                    mesg(f"Patching instance variable: {qualified_name}")
                    self.patch_instance_property(cls, name, member, member_info, level='instance')
            elif member_type == MemberType.FUNCTION:
                # Functions inside classes are treated as methods
                if level == MemberLevel.INSTANCE:
                    mesg(f"Patching instance function: {qualified_name}")
                    self.patch_instance_method(cls, name, member, member_info)
                else:
                    mesg(f"Patching static class function: {qualified_name}")
                    self.patch_static_method(cls, name, member, member_info)
            elif member_type == MemberType.SPECIAL_METHOD:
                if level == MemberLevel.INSTANCE:
                    mesg(f"Patching special instance method: {qualified_name}")
                    self.patch_instance_method(cls, name, member, member_info)
                elif level == MemberLevel.CLASS:
                    mesg(f"Patching special class method: {qualified_name}")
                    self.patch_class_method(cls, name, member, member_info)
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
                    # Create member_info for the function
                    member_info = {
                        'name': name,
                        'member': obj,
                        'member_type': MemberType.FUNCTION,
                        'level': MemberLevel.MODULE,
                        'class_name': module.__name__,
                        'class_object': module
                    }
                    # Patch module functions
                    patcher.patch_module_function(module, name, obj, member_info)
                elif not callable(obj):
                    # Create member_info for the variable
                    member_info = {
                        'name': name,
                        'member': obj,
                        'member_type': MemberType.VARIABLE,
                        'level': MemberLevel.MODULE,
                        'class_name': module.__name__,
                        'class_object': module
                    }
                    # Patch module variables
                    mesg(f"Patching module variable: {name_dot(module, name)}")
                    patcher.patch_module_property(module, name, obj, member_info)
            except Exception as e:
                mesg(f"Error patching {name_dot(module, name)}: {e}")

    def patch_class_init(self, cls):
        """Patch the class __init__ to handle instance variables."""
        original_init = cls.__init__
        overrider = self

        def capture_init_variables_once(instance, *args, **kwargs):
            """Wrapper for the class __init__ that patches instance variables."""
            mesg(f"Initializing {cls.__name__} with args {args} and kwargs {kwargs}")

            # Call the original __init__ method
            result = original_init(instance, *args, **kwargs)
            # Undo this instance variable system
            setattr(cls, '__init__', original_init)
            # Store the original __init__ method for unpatching
            self.original_methods[(cls, '__init__')] = original_init
            # Patch instance variables after __init__ completes
            overrider.patch_instance_variables(instance)
            return result

        # Ensure the method is callable and handle function vs bound method
        if cls not in self.attempted_to_capture_init_variables_on_classes:
            self.attempted_to_capture_init_variables_on_classes.add(cls)
            # Replace the original __init__ with capture_init_variables_once
            setattr(cls, '__init__', capture_init_variables_once)
            # Store the original __init__ method for unpatching
            self.original_methods[(cls, '__init__')] = original_init

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
        self.patch_member_info(cls, member_info)

    def patch_instance_property(self, target_class, property_name, original_property, member_info, level):
        """Patch an instance property to allow observation of gets and sets."""
        try:
            mesg(f"Patching instance property: {property_name} in {target_class.__name__}")

            # Initialize the 'suspend_trace' flag for this method
            suspend_trace = False
            overrider = self

            # Create getter and setter functions using the captured observer
            def patched_getter(instance):
                nonlocal suspend_trace
                #mesg(f"Calling patched_getter for '{property_name}' in {target_class.__name__}")

                if rust_mode or suspend_trace:
                    # If tracing is suspended or rust_mode is True, directly return the original value
                    if isinstance(original_property, property) and original_property.fget:
                        return original_property.fget(instance)
                    else:
                        return getattr(instance, '__dict__', {}).get(property_name, None)

                try:
                    # Suspend tracing while calling check_override
                    suspend_trace = True
                    try:
                        result = overrider.check_override(member_info, 'get', instance, target_class, property_name,
                                                          original_property, level=level)
                    finally:
                        suspend_trace = False

                    if isinstance(result, dict) and result.get('do_not_really_get', False):
                        return result.get('return_value')

                    # Access the current value, handling __dict__ and original getter cases
                    if property_name in instance.__dict__:
                        current_value = instance.__dict__[property_name]
                    elif isinstance(original_property, property) and original_property.fget:
                        current_value = original_property.fget(instance)
                    else:
                        current_value = getattr(instance, property_name, None)

                    return current_value

                except Exception as e:
                    mesg(f"Error getting property '{property_name}' in {target_class.__name__}: {e}")
                    raise

            def patched_setter(instance, new_value):
                nonlocal suspend_trace
                mesg(f"Calling patched_setter for '{property_name}' in {target_class.__name__} with value {new_value}")

                if rust_mode or suspend_trace:
                    if isinstance(original_property, property) and original_property.fset:
                        original_property.fset(instance, new_value)
                    else:
                        instance.__dict__[property_name] = new_value
                    return

                try:
                    # Suspend tracing while calling check_override
                    suspend_trace = True
                    try:
                        # Notify the observer before setting the new value
                        result = overrider.check_override(member_info, 'set', instance, target_class, property_name,
                                                          new_value, level=level)
                    finally:
                        suspend_trace = False

                    if isinstance(result, dict):
                        if result.get('do_not_really_set', False):
                            return
                        if 'return_value' in result:
                            new_value = result.get('return_value')

                    # Set the value using the original setter if it exists, or directly update __dict__
                    if isinstance(original_property, property) and original_property.fset:
                        original_property.fset(instance, new_value)
                    else:
                        instance.__dict__[property_name] = new_value  # Directly set in __dict__

                except Exception as e:
                    mesg(f"Error setting property '{property_name}' in {target_class.__name__}: {e}")
                    raise

            # Apply the new property
            new_property = property(fget=patched_getter, fset=patched_setter)
            # Store the original property before replacing
            self.original_methods[(target_class, property_name)] = original_property
            setattr(target_class, property_name, new_property)

            mesg(f"Successfully patched instance property: {property_name} in {target_class.__name__}")

        except Exception as e:
            mesg(f"Failed to patch instance property '{property_name}' in {target_class.__name__}: {e}")
            raise

    # Patching module properties
    def patch_module_property(self, module, property_name, value, member_info):
        """Patch module properties to allow observation of gets and sets."""
        private_name = f"_{property_name}"
        try:
            original_attribute = getattr(module, property_name, None)
            # Store the original attribute before replacing
            self.original_methods[(module, property_name)] = original_attribute

            setattr(module, private_name, value)
            mesg(f"Patching module property: {property_name} in {module.__name__}")

            def module_getter():
                try:
                    current_value = getattr(module, private_name)
                    if rust_mode:
                        return current_value
                    result = self.check_override(member_info, 'get', module, property_name, current_value, level='module')
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
                    result = self.check_override(None, 'set', module, property_name, new_value, level='module')
                    if isinstance(result, dict):
                        if result.get('do_not_really_set', False):
                            return
                        if 'return_value' in result:
                            new_value = result.get('return_value')
                    setattr(module, private_name, new_value)
                except Exception as e:
                    mesg(TRACE, f"Error setting module property '{name_dot(module, property_name)}': {e}")
                    raise

            setattr(module, property_name, property(fget=module_getter, fset=module_setter))
        except Exception as e:
            mesg(TRACE, f"Failed to patch module property '{name_dot(module, property_name)}': {e}")
            raise

    def patch_instance_method(self, cls, method_name, original_method, member_info):
        """Patch an instance method to allow observation of calls."""
        mesg(f"Patching instance method: {name_dot(cls, method_name)}")

        # Initialize the 'suspend_trace' flag for this method
        suspend_trace = False
        overrider = self

        def patched_method(inst, *args, **kwargs):
            nonlocal suspend_trace
            try:
                if rust_mode or suspend_trace:
                    # If tracing is suspended or rust_mode is True, directly call the original method
                    return original_method(inst, *args, **kwargs)

                # Suspend tracing while calling check_override
                suspend_trace = True
                try:
                    # Before Call Event
                    result_before = overrider.check_override(member_info, 'before_call', inst, method_name, *args,
                                                             **kwargs)
                finally:
                    suspend_trace = False

                if isinstance(result_before, dict):
                    if result_before.get('do_not_really_call', False):
                        return result_before.get('return_value')

                # Call the original method
                result = original_method(inst, *args, **kwargs)

                # Suspend tracing while calling check_override
                suspend_trace = True
                try:
                    # After Call Event
                    result_after = overrider.check_override(member_info, 'after_call', inst, method_name, result)
                finally:
                    suspend_trace = False

                if isinstance(result_after, dict) and result_after.get('override_value', False):
                    return result_after.get('return_value')

                return result
            except Exception as e:
                mesg(f"Error in patched method '{name_dot(cls, method_name)}': {e}")
                raise

        # Replace the method on the class
        setattr(cls, method_name, patched_method)
        # Store the original method for unpatching
        self.original_methods[(cls, method_name)] = original_method

    def patch_class_method(self, cls, method_name, method, member_info):
        """Patch a class method to allow observation of calls."""
        original_method = method.__func__
        mesg(f"Patching class method: {name_dot(cls, method_name)}")

        # Initialize the 'suspend_trace' flag for this method
        suspend_trace = False
        overrider = self

        def patched_class_method(cls_self, *args, **kwargs):
            nonlocal suspend_trace
            try:
                if rust_mode or suspend_trace:
                    # If tracing is suspended or rust_mode is True, directly call the original method
                    return original_method(cls_self, *args, **kwargs)

                # Suspend tracing while calling check_override
                suspend_trace = True
                try:
                    # Before Call Event
                    result_before = overrider.check_override(member_info, 'before_call', cls_self, method_name, *args,
                                                             **kwargs)
                finally:
                    suspend_trace = False

                if isinstance(result_before, dict):
                    if result_before.get('do_not_really_call', False):
                        return result_before.get('return_value')

                # Call the original method
                result = original_method(cls_self, *args, **kwargs)

                # Suspend tracing while calling check_override
                suspend_trace = True
                try:
                    # After Call Event
                    result_after = overrider.check_override(member_info, 'after_call', cls_self, method_name, result)
                finally:
                    suspend_trace = False

                if isinstance(result_after, dict) and result_after.get('override_value', False):
                    return result_after.get('return_value')

                return result
            except Exception as e:
                mesg(f"Error in patched class method '{name_dot(cls, method_name)}': {e}")
                raise

        # Replace the method on the class
        setattr(cls, method_name, classmethod(patched_class_method))
        # Store the original method for unpatching
        self.original_methods[(cls, method_name)] = method

    def patch_static_method(self, cls, method_name, method, member_info):
        """Patch a static method to allow observation of calls."""
        original_method = method.__func__ if isinstance(method, staticmethod) else method
        mesg(f"Patching static method: {name_dot(cls, method_name)}")

        # Initialize the 'suspend_trace' flag for this method
        suspend_trace = False
        overrider = self

        def patched_static_method(*args, **kwargs):
            nonlocal suspend_trace
            try:
                if rust_mode or suspend_trace:
                    # If tracing is suspended or rust_mode is True, directly call the original method
                    return original_method(*args, **kwargs)

                # Suspend tracing while calling check_override
                suspend_trace = True
                try:
                    # Before Call Event
                    result_before = overrider.check_override(member_info, 'before_call', cls, method_name, *args,
                                                             **kwargs)
                finally:
                    suspend_trace = False

                if isinstance(result_before, dict):
                    if result_before.get('do_not_really_call', False):
                        return result_before.get('return_value')

                # Call the original method
                result = original_method(*args, **kwargs)

                # Suspend tracing while calling check_override
                suspend_trace = True
                try:
                    # After Call Event
                    result_after = overrider.check_override(member_info, 'after_call', cls, method_name, result)
                finally:
                    suspend_trace = False

                if isinstance(result_after, dict) and result_after.get('override_value', False):
                    return result_after.get('return_value')

                return result
            except Exception as e:
                mesg(f"Error in patched static method '{name_dot(cls, method_name)}': {e}")
                raise

        # Replace the method on the class
        setattr(cls, method_name, staticmethod(patched_static_method))
        # Store the original method for unpatching
        self.original_methods[(cls, method_name)] = method

    def patch_module_function(self, module, function_name, function, member_info):
        """Patch a module-level function to allow observation of calls."""
        original_function = function
        mesg(f"Patching module function: {name_dot(module, function_name)} {signature(original_function)}")

        # Initialize the 'suspend_trace' flag for this function
        suspend_trace = False
        overrider = self

        def patched_function(*args, **kwargs):
            nonlocal suspend_trace
            try:
                if rust_mode or suspend_trace:
                    # If tracing is suspended or rust_mode is True, directly call the original function
                    return original_function(*args, **kwargs)

                # Suspend tracing while calling check_override
                suspend_trace = True
                try:
                    # Before Call Event
                    result_before = overrider.check_override(member_info, 'before_call', module, function_name, *args,
                                                             **kwargs, level='module')
                finally:
                    suspend_trace = False

                if isinstance(result_before, dict):
                    if result_before.get('do_not_really_call', False):
                        return result_before.get('return_value')

                # Call the original function
                result = original_function(*args, **kwargs)

                # Suspend tracing while calling check_override
                suspend_trace = True
                try:
                    # After Call Event
                    result_after = overrider.check_override(member_info, 'after_call', module, function_name, result,
                                                            level='module')
                finally:
                    suspend_trace = False

                if isinstance(result_after, dict) and result_after.get('override_value', False):
                    return result_after.get('return_value')

                return result
            except Exception as e:
                mesg(f"Error in patched function '{name_dot(module, function_name)}': {e}")
                raise

        # Replace the function on the module
        setattr(module, function_name, patched_function)
        # Store the original function for unpatching
        self.original_methods[(module, function_name)] = original_function

    def patch_static_property(self, cls, property_name, value, member_info):
        """Patch class (static) properties to allow observation of gets and sets."""
        original_attribute = getattr(cls, property_name, None)
        mesg(f"Patching static property: {name_dot(cls, property_name)}")

        # Initialize the 'suspend_trace' flag for this method
        suspend_trace = False
        overrider = self

        # Create getter
        def class_getter():
            nonlocal suspend_trace
            try:
                if rust_mode or suspend_trace:
                    return value
                # Suspend tracing while calling check_override
                suspend_trace = True
                try:
                    current_value = value
                    result = overrider.check_override(member_info, 'get', cls, property_name, current_value,
                                                      level="class")
                finally:
                    suspend_trace = False

                if isinstance(result, dict) and result.get('do_not_really_get', False):
                    return result.get('return_value')
                return current_value
            except Exception as e:
                mesg(TRACE, f"Error getting static property '{name_dot(cls, property_name)}': {e}")
                raise

        # Create setter
        def class_setter(new_value):
            nonlocal suspend_trace, value
            try:
                if rust_mode or suspend_trace:
                    value = new_value
                    return
                # Suspend tracing while calling check_override
                suspend_trace = True
                try:
                    result = overrider.check_override(member_info, 'set', cls, property_name, new_value, level="class")
                finally:
                    suspend_trace = False

                if isinstance(result, dict):
                    if result.get('do_not_really_set', False):
                        return
                    if 'return_value' in result:
                        new_value = result.get('return_value')
                    if result.get('really_set', False):
                        new_value = result.get('new_value', new_value)  # Set to the new value if provided

                value = new_value
            except Exception as e:
                mesg(TRACE, f"Error setting static property '{name_dot(cls, property_name)}': {e}")
                raise

        # Replace the class attribute with a property descriptor
        setattr(cls, property_name, property(fget=class_getter, fset=class_setter))
        # Store the original attribute for unpatching
        self.original_methods[(cls, property_name)] = original_attribute

    def unpatch_system(self):
        """Restore all patched attributes to their original values."""
        mesg(f"Unpatching system by restoring {len(self.original_methods)} attributes")
        for (target, attribute_name), original_attribute in reversed(self.original_methods.items()):
            setattr(target, attribute_name, original_attribute)
        # Clear the original_methods dictionary
        self.original_methods.clear()

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
    """Determine the implementation status of a member."""
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
# from metta_python_patcher import Observer, mesg, name_dot

# Flag to switch between using simple `if` or `assert`
USE_ASSERT = False

# ANSI color codes for colored output
CYAN = "\033[96m"
RESET = "\033[0m"
RED = "\033[91m"
GREEN = "\033[92m"

def commentary(message):
    """Function to print a message in cyan for debug commentary."""
    print(f"{CYAN}{message}{RESET}")

def check_equal(expected, actual, message=""):
    """Check if two values are equal, either using assert or if."""

    commentary(f"DEBUG: {message} - Expected: {expected}, Actual: {actual}")
    if USE_ASSERT:
        assert expected == actual, f"{message} Expected: {expected}, got: {actual}"
    else:
        if expected != actual:
            print(f"{RED}ERROR: {message} Expected: {expected}, got: {actual}{RESET}")
        else:
            print(f"{GREEN}SUCCESS: {message} Expected and got: {expected}{RESET}")


# Test case for demoHyperonPy
def test_demoHyperonPy():
    from metta_python_patcher import Overrider, mesg, name_dot
    import hyperonpy
    from hyperon.runner import MeTTa
    from hyperon.atoms import GroundedAtom, VariableAtom, SymbolAtom, ExpressionAtom, ValueObject, OperationObject, \
        MatchableObject

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
        #close_paren_after_patch = close_paren_after_patch.fget(hyperonpy)

    check_equal(const, close_paren_after_patch, "hyperonpy.CLOSE_PAREN")

    commentary("Unpatching the system to restore original behavior")
    overrider.unpatch_system()

    commentary("Running MeTTa expression after unpatching to ensure original behavior")
    result_unpatched = metta.run("!(+ 1 1)")
    check_equal("[[2]]", repr(result_unpatched), "MeTTa run result after unpatching")

    commentary("Finished with test_demoHyperonPy\n\n")

def demoAtoms():
    from metta_python_patcher import Overrider, mesg, name_dot
    import hyperonpy
    from hyperon.runner import MeTTa
    from hyperon.atoms import GroundedAtom, VariableAtom, SymbolAtom, ExpressionAtom, ValueObject, OperationObject, \
        MatchableObject

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

    commentary("Unpatching the system to restore original behavior")
    overrider.unpatch_system()

    commentary("Creating a new ValueObject instance after unpatching")
    atom_unpatched = ValueObject("I am a String")

    commentary("Accessing 'value' property after unpatching, expecting 'I am a String'")
    check_equal("I am a String", atom_unpatched.value, "ValueObject value after unpatching")

    commentary("Finished with demoAtoms\n\n")

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
    commentary(
        "Creating an instance of MyClass. Expecting instance_var to be initialized to 10 and value to 'inst_value'")
    obj = MyClass()

    commentary(
        "Accessing the 'my_instance_property' property of the MyClass instance, expecting it to return 10 (initial value)")
    check_equal(10, obj.my_instance_property, "MyClass initial my_instance_property")

    commentary("Setting 'my_instance_property' to 200. This will modify the internal 'instance_var' of MyClass")
    obj.my_instance_property = 200

    commentary("Re-accessing 'my_instance_property' after modification, expecting it to return 200")
    check_equal(200, obj.my_instance_property, "MyClass updated my_instance_property")

    commentary("Calling 'instance_method' of MyClass, expecting it to return 'instance_var' (which should now be 200)")
    check_equal(200, obj.instance_method(), "MyClass instance_method result")

    commentary(
        "Getting the string representation of MyClass using 'repr', expecting it to return the 'value' attribute")
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

    commentary("Unpatching the system to restore original behavior")
    overrider.unpatch_system()

    commentary("Creating a new instance of MyClass after unpatching")
    obj_unpatched = MyClass()

    commentary("Accessing 'my_instance_property' after unpatching, expecting it to return 10")
    check_equal(10, obj_unpatched.my_instance_property, "MyClass my_instance_property after unpatching")

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

    commentary(
        "Accessing 'test_property' of the TestClassV2 instance, expecting it to return 10 (initial value of 'instance_var')")
    check_equal(10, obj.test_property, "TestClassV2 initial test_property")

    commentary("Setting 'test_property' to 200, which updates 'instance_var' to 200")
    obj.test_property = 200

    commentary("Re-accessing 'test_property' after modification, expecting it to return 200")
    check_equal(200, obj.test_property, "TestClassV2 updated test_property")

    commentary("Setting 'test_property' again to 500, this should update 'instance_var' to 500")
    obj.test_property = 500

    commentary("Verifying that 'test_property' now returns 500 after the second update")
    check_equal(500, obj.test_property, "TestClassV2 second update test_property")

    commentary("Unpatching the system to restore original behavior")
    overrider.unpatch_system()

    commentary("Creating a new instance of TestClassV2 after unpatching")
    obj_unpatched = TestClassV2()

    commentary("Accessing 'test_property' after unpatching, expecting it to return 10")
    check_equal(10, obj_unpatched.test_property, "TestClassV2 test_property after unpatching")

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
def test_method_override():
    commentary("Setting up a fresh overrider system for method override testing in OverrideClass")
    overrider = Overrider(False)  # Ensure everything is set up

    commentary("Subscribing to 'before_call' to override 'multiply' method when called with argument 5")

    def before_multiply_override(*args):
        if len(args) >= 3:
            instance, method_name = args[0], args[1]
            mesg(f"[Override] Before calling {name_dot(instance.__class__, method_name)}")
            # Check if we are calling 'multiply'
            if method_name == "multiply":
                method_args = args[2:]  # Unpack the method's actual arguments here
                mesg(f"[Override] Method arguments: {method_args}")
                if len(method_args) > 0 and method_args[0] == 5:
                    commentary(f"Overriding {name_dot(instance.__class__, method_name)} to return 100 when input is 5")
                    return {'do_not_really_call': True, 'return_value': 100}  # Override return value if argument is 5
        else:
            mesg(f"[{RED}] Not enough arguments provided to before_multiply_override")
        return {}  # Return empty dict to proceed normally

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

    commentary("Unpatching the system to restore original behavior")
    overrider.unpatch_system()

    commentary("Calling 'multiply' with 5 after unpatching, expecting it to return 10 (standard behavior)")
    check_equal(10, obj.multiply(5), "OverrideClass multiply(5) after unpatching")

    commentary("Finished with test_method_override\n\n")

def test_property_get_override():
    commentary("Setting up a fresh overrider system for property getter override in OverrideClass")
    overrider = Overrider(False)  # Set up the overrider and patcher

    commentary("Subscribing to 'get' to override 'dynamic_property' to always return 999")
    def override_dynamic_property_get(*args):
        if len(args) >= 3:
            instance, property_name = args[0], args[1]
            mesg(f"[Override] Overriding {name_dot(instance.__class__, property_name)} getter")
            if property_name == "dynamic_property":
                # Return the overridden value for dynamic_property
                return {'do_not_really_get': True, 'return_value': 999}
        else:
            mesg(f"[{RED}] Not enough arguments provided to override_dynamic_property_get")
        return {}

    overrider.add_override('get', override_dynamic_property_get, level='instance')

    commentary("Marking OverrideClass for inspection")
    overrider.mark_class(OverrideClass)

    commentary("Attempting to patch all members of OverrideClass")
    overrider.monkey_patch_members()

    commentary("Creating an instance of OverrideClass, which initializes 'instance_var' to 10")
    obj = OverrideClass()

    commentary("Accessing 'dynamic_property' expecting the overridden value of 999 (instead of 10)")
    value = obj.dynamic_property
    mesg(f"Retrieved value of dynamic_property: {value}")
    check_equal(999, value, "OverrideClass dynamic_property override")

    commentary("Unpatching the system to restore original behavior")
    overrider.unpatch_system()

    commentary("Accessing 'dynamic_property' after unpatching, expecting it to return the original value of 10")
    value_unpatched = obj.dynamic_property
    mesg(f"Retrieved value of dynamic_property after unpatching: {value_unpatched}")
    check_equal(10, value_unpatched, "OverrideClass dynamic_property after unpatching")

    commentary("Finished with test_property_get_override\n\n")

def test_property_set_override():
    commentary("Setting up a fresh overrider system for property setter override in OverrideClass")
    overrider = Overrider(False)  # Ensure everything is set up

    commentary("Subscribing to 'set' to block setting 'dynamic_property' to 300")
    def set_property_override(*args):
        if len(args) >= 4:
            instance, property_name, new_value = args[0], args[1], args[2]
            mesg(f"[Override] Setting {name_dot(instance.__class__, property_name)} to {new_value}")
            if property_name == "dynamic_property" and new_value == 300:
                commentary(f"Blocking {name_dot(instance.__class__, property_name)} from being set to 300")
                return {'do_not_really_set': True}  # Block setting if the new value is 300
        else:
            mesg(f"[{RED}] Not enough arguments provided to set_property_override")
        return {}

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

    commentary("Unpatching the system to restore original behavior")
    overrider.unpatch_system()

    commentary("Setting 'dynamic_property' to 300 after unpatching (should succeed)")
    obj.dynamic_property = 300
    check_equal(300, obj.dynamic_property, "OverrideClass dynamic_property set to 300 after unpatching")

    commentary("Finished with test_property_set_override\n\n")

def test_static_method_override():
    commentary("Setting up a fresh overrider system for static method override in OverrideClass")
    overrider = Overrider()  # Ensure everything is set up

    commentary("Subscribing to 'before_call' to override 'static_method' to return 777")
    def before_static_method_override(*args):
        if len(args) >= 2:
            cls, method_name = args[0], args[1]
            mesg(f"[Override] Before calling {name_dot(cls, method_name)}")
            if method_name == "static_method":
                commentary(f"Overriding {name_dot(cls, method_name)} to return 777")
                return {'do_not_really_call': True, 'return_value': 777}  # Override static method return value
        else:
            mesg(f"[{RED}] Not enough arguments provided to before_static_method_override")
        return {}

    overrider.add_override('before_call', before_static_method_override, level='class')

    commentary("Marking OverrideClass for inspection")
    overrider.mark_class(OverrideClass, also_static=True)

    commentary("Attempting to patch all members of OverrideClass")
    overrider.monkey_patch_members()

    commentary("Calling 'static_method', expecting the overridden return value of 777")
    check_equal(777, OverrideClass.static_method(), "OverrideClass static_method override")

    commentary("Unpatching the system to restore original behavior")
    overrider.unpatch_system()

    commentary("Calling 'static_method' after unpatching, expecting the original return value of 555")
    check_equal(555, OverrideClass.static_method(), "OverrideClass static_method after unpatching")

    commentary("Finished with test_static_method_override\n\n")

def test_base_method_override():
    commentary("Setting up a fresh overrider system for base method override in DerivedClassForOverride")
    overrider = Overrider()

    commentary("Subscribing to 'before_call' to override 'base_method' to return 100 if called with 5")
    def before_base_method_override(*args):
        if len(args) >= 3:
            instance, method_name = args[0], args[1]
            method_args = args[2:]
            mesg(f"[Override] Before calling {name_dot(instance.__class__, method_name)}")
            if method_name == "base_method" and method_args == (5,):
                commentary(f"Overriding {name_dot(instance.__class__, method_name)} to return 100 when input is 5")
                return {'do_not_really_call': True, 'return_value': 100}  # Override return value if argument is 5
        else:
            mesg(f"[{RED}] Not enough arguments provided to before_base_method_override")
        return {}

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

    commentary("Unpatching the system to restore original behavior")
    overrider.unpatch_system()

    commentary("Calling 'derived_method' with 5 after unpatching, expecting standard behavior to return 11 (5 * 2 + 1)")
    check_equal(11, obj.derived_method(5), "DerivedClassForOverride derived_method(5) after unpatching")

    commentary("Finished with test_base_method_override\n\n")

def test_properties():
    # Create a dummy module to test module-level properties
    module_name = 'dummy_module'
    dummy_module = types.ModuleType(module_name)
    sys.modules[module_name] = dummy_module

    # Add a module-level variable
    dummy_module.module_var = 10

    # Define a class with various property implementations
    class TestClass:
        # Class variable
        class_var = 20

        def __init__(self):
            # Instance variable
            self.instance_var = 30
            # Property with getter and setter
            self._value = 0

        @property
        def prop(self):
            return self._value

        @prop.setter
        def prop(self, val):
            self._value = val

    # Instantiate the Overrider
    overrider = Overrider()
    overrider.mark_members(dummy_module)
    overrider.mark_class(TestClass)
    overrider.monkey_patch_members()

    # Define callbacks for 'set' and 'get' events
    def modify_before_set(*args):
        if len(args) >= 4:
            instance_or_class, property_name, new_value = args[0], args[1], args[2]
            mesg(f"[Override] Before setting {name_dot(instance_or_class, property_name)} to {new_value}")
            current_value = getattr(instance_or_class, property_name)
            new_value = current_value + new_value
            return {'return_value': new_value}
        else:
            mesg(f"[{RED}] Not enough arguments provided to modify_before_set")
        return {}

    def log_after_get(*args):
        if len(args) >= 3:
            instance_or_class, property_name, current_value = args[0], args[1], args[2]
            mesg(f"After getting {name_dot(instance_or_class, property_name)}: {current_value}")
        else:
            mesg(f"[{RED}] Not enough arguments provided to log_after_get")

    # Subscribe to the 'set' and 'get' events
    overrider.add_override('set', modify_before_set, level='instance')
    overrider.add_override('set', modify_before_set, level='class')
    overrider.add_override('set', modify_before_set, level='module')
    overrider.add_override('get', log_after_get, level='instance')
    overrider.add_override('get', log_after_get, level='class')
    overrider.add_override('get', log_after_get, level='module')

    # Testing instance property
    print("\nTesting instance property:")
    obj = TestClass()
    check_equal(0, obj.prop, "Initial obj.prop")
    obj.prop = 5  # Should add current value and new value: 0 + 5 = 5
    check_equal(5, obj.prop, "Updated obj.prop to 5")
    obj.prop = 7  # Should add current value and new value: 5 + 7 = 12
    check_equal(12, obj.prop, "Updated obj.prop to 12")

    # Testing instance variable
    print("\nTesting instance variable:")
    check_equal(30, obj.instance_var, "Initial obj.instance_var")
    obj.instance_var = 5  # Should add current value and new value: 30 + 5 = 35
    check_equal(35, obj.instance_var, "Updated obj.instance_var to 35")
    obj.instance_var = 10  # Should add current value and new value: 35 + 10 = 45
    check_equal(45, obj.instance_var, "Updated obj.instance_var to 45")

    # Testing class variable
    print("\nTesting class variable:")
    check_equal(20, TestClass.class_var, "Initial TestClass.class_var")
    TestClass.class_var = 5  # Should add current value and new value: 20 + 5 = 25
    check_equal(25, TestClass.class_var, "Updated TestClass.class_var to 25")
    TestClass.class_var = 10  # Should add current value and new value: 25 + 10 = 35
    check_equal(35, TestClass.class_var, "Updated TestClass.class_var to 35")

    # Testing module variable
    print("\nTesting module variable:")
    check_equal(10, dummy_module.module_var, "Initial dummy_module.module_var")
    dummy_module.module_var = 5  # Should add current value and new value: 10 + 5 = 15
    check_equal(15, dummy_module.module_var, "Updated dummy_module.module_var to 15")
    dummy_module.module_var = 10  # Should add current value and new value: 15 + 10 = 25
    check_equal(25, dummy_module.module_var, "Updated dummy_module.module_var to 25")

    commentary("Unpatching the system to restore original behavior")
    overrider.unpatch_system()

def test_properties_0002():
    class TestClass:
        def __init__(self):
            self._value = 42

        @property
        def value(self):
            return self._value

        def example_method(self):
            print(f"Value is {self._value}")

    overrider = Overrider()
    overrider.mark_class(TestClass)

    def before_set_callback(*args):
        # Before setting the readonly property, modify the value
        if len(args) >= 4:
            instance, property_name, new_value = args[0], args[1], args[2]
            mesg(f"[Override] Before setting {name_dot(instance.__class__, property_name)} to {new_value}")
            return {'return_value': new_value + 1}  # Modifying the new value
        else:
            mesg(f"[{RED}] Not enough arguments provided to before_set_callback")
        return {}

    overrider.add_override('set', before_set_callback, level='instance')
    overrider.monkey_patch_members()

    obj = TestClass()
    print(f"Original value: {obj.value}")  # Should print 42
    obj.value = 50  # This should trigger before_set and modify the value
    print(f"Updated value: {obj.value}")  # Should print 51 due to the before_set callback

    commentary("Unpatching the system to restore original behavior")
    overrider.unpatch_system()

import platform

# Main function to execute all tests
def main():
    commentary("Starting the main test suite")
    if platform.system() == "Linux":  # Check if we are on Linux
        test_demoHyperonPy()  # Test the HyperonPy and MeTTa patching
        demoAtoms()  # Inspect and patch Atom classes
    demoMyClass()  # Inspect and patch MyClass
    test_TestClassV2()  # Test property access and modification for TestClassV2
    test_method_override()  # Test method overriding
    test_property_get_override()  # Test property getter override
    test_property_set_override()  # Test property setter override
    test_static_method_override()  # Test static method override
    test_base_method_override()  # Test base method overriding
    test_properties()
    test_properties_0002()
    commentary("Finished executing the main test suite")

# Execute main if run as a script
if __name__ == "__main__":
    main()


