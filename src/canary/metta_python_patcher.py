import inspect
from enum import Enum
import sys
import types
import traceback

# Global variables
suspend_trace = False
rust_mode = False  # Set to False as default
TRACE = True  # Assuming this is a logging level

def name_dot(module, name):
    if module is None:
        return name
    return f"{module.__name__}.{name}"

import inspect

def signature(obj):
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


def ignore_exception(*args, **kwargs):
    pass

def mesg(*args, **kwargs):
    """Simple message/logging function placeholder."""
    print(*args, **kwargs)

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

# 1. Observer Class
class Observer:
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
        if suspend_trace:
            return

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

        # Check if there are any observers subscribed to this event key
        if event_key in self.observers:
            for callback in self.observers[event_key]:
                result = callback(*args)
                if result is not None:
                    return result

        # Default return if no observer modifies the value
        return kwargs.get('default_value')

# 2. Helper Functions for Instance Getters/Setters
def create_instance_getter(original_property, property_name, target, observer, level):
    """A function to handle the instance getter logic."""
    def instance_getter(self):
        try:
            current_value = original_property.fget(self)
            result = observer.notify(
                original_property, 'get', target, property_name, current_value, level=level
            )
            if isinstance(result, dict) and result.get('do_not_really_get', False):
                return result.get('return_value')
            return current_value
        except Exception as e:
            mesg(TRACE, f"Error getting instance property '{property_name}' in {target.__name__}: {e}")
            raise
    return instance_getter

def create_instance_setter(original_property, property_name, target, observer, level):
    """A function to handle the instance setter logic."""
    def instance_setter(self, new_value):
        try:
            result = observer.notify(
                original_property, 'set', target, property_name, new_value, level=level
            )
            if isinstance(result, dict) and result.get('do_not_really_set', False):
                return
            new_value = result if result is not None else new_value
            original_property.fset(self, new_value)
        except Exception as e:
            mesg(TRACE, f"Error setting instance property '{property_name}' in {target.__name__}: {e}")
            raise
    return instance_setter

# 3. MonkeyPatcher Class
class MonkeyPatcher:
    def __init__(self, observer):
        self.observer = observer
        self.patched_objects = {}
        self.patched_modules = {}
        self.patched_instance_classes = {}
        self.patched_static_classes = {}

    def patch_instance_property(self, target_class, property_name, original_property, level):
        """Patch an instance property to allow observation of gets and sets."""
        try:
            #mesg(TRACE, f"[Trace] Patching instance property: {property_name} in {target_class.__name__}")

            # Create the getter and setter functions
            instance_getter = create_instance_getter(original_property, property_name, target_class, self.observer, level)

            # Check if the original property has a setter
            if hasattr(original_property, 'fset') and original_property.fset is not None:
                instance_setter = create_instance_setter(original_property, property_name, target_class, self.observer, level)
                new_property = property(fget=instance_getter, fset=instance_setter)
            else:
                new_property = property(fget=instance_getter)

            # Replace the property on the class
            setattr(target_class, property_name, new_property)

            mesg(f"Patching instance property: {property_name} in {target_class.__name__}")

        except Exception as e:
            mesg(f"Failed to patch instance property '{property_name}' in {target_class.__name__}: {e}")
            raise

    def patch_module_property(self, module, property_name, value):
        """Patch module properties to allow observation of gets and sets with exception handling."""
        private_name = f"_{property_name}"
        try:
            setattr(module, private_name, value)
            mesg(TRACE, f"[Trace] Patching module property: {property_name} in {module.__name__}")

            def module_getter():
                try:
                    current_value = getattr(module, private_name)
                    if rust_mode:
                        return current_value
                    result = self.observer.notify(value, 'get', module, property_name, current_value, level='module')
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
                    result = self.observer.notify(value, 'set', module, property_name, new_value, level='module')
                    if isinstance(result, dict) and result.get('do_not_really_set', False):
                        return
                    setattr(module, private_name, result if result is not None else new_value)
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
            result = self.observer.notify(
                original_method, 'before_call', target_class, method_name, args, kwargs, level='instance'
            )
            if isinstance(result, dict) and result.get('do_not_really_call', False):
                return result.get('return_value')
            elif isinstance(result, tuple):
                modified_args, modified_kwargs = result
            else:
                modified_args, modified_kwargs = args, kwargs

            try:
                result = original_method(instance_self, *modified_args, **modified_kwargs)
            except Exception as e:
                mesg(f"Error in instance method {method_name}: {e}")
                raise

            modified_result = self.observer.notify(
                original_method, 'after_call', target_class, method_name, result, level='instance'
            )
            return modified_result if modified_result is not None else result

        # Print the signature
        #mesg(f"Patching instance method: {name_dot(target_class, method_name)} {signature(original_method)}")
        setattr(target_class, method_name, patched_method)

    def patch_class_method(self, target_class, method_name, method):
        """Patch a class method to allow observation of calls."""
        # Extract the unbound function from the classmethod object
        original_method = method.__func__

        def patched_class_method(cls, *args, **kwargs):
            result = self.observer.notify(
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

            modified_result = self.observer.notify(
                original_method, 'after_call', target_class, method_name, result, level='class'
            )
            return modified_result if modified_result is not None else result

        setattr(target_class, method_name, classmethod(patched_class_method))
        #mesg(f"Monkey-patched class method {name_dot(target_class, method_name)}")

    def patch_static_method(self, target_class, method_name, method):
        """Patch a static method to allow observation of calls."""
        original_method = method

        def patched_static_method(*args, **kwargs):
            result = self.observer.notify(
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

            modified_result = self.observer.notify(
                original_method, 'after_call', target_class, method_name, result, level='class'
            )
            return modified_result if modified_result is not None else result

        setattr(target_class, method_name, staticmethod(patched_static_method))
        #mesg(f"Monkey-patched static method {name_dot(target_class, method_name)}")

    def patch_module_function(self, module, function_name, function):
        """Patch a function to allow observation of calls."""
        original_function = function

        def patched_function(*args, **kwargs):
            result = self.observer.notify(
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

            modified_result = self.observer.notify(
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
            mesg(TRACE, f"[Trace] Patching static property: {name_dot(cls,property_name)}")

            # Create getter
            def class_getter():
                try:
                    current_value = getattr(cls, private_name)  # Retrieve the private value
                    #mesg(TRACE, f"Getting static property '{property_name}' with value {current_value}")
                    if rust_mode:
                        return current_value
                    result = self.observer.notify(value, 'get', cls, property_name, current_value, level="class")
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
                    result = self.observer.notify(value, 'set', cls, property_name, new_value, level="class")
                    
                    # Handle 'do_not_really_set' and 'really_set' options
                    if isinstance(result, dict):
                        if result.get('do_not_really_set', False):
                            return  # Do not set the value if the callback dictates not to
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
            #mesg(TRACE, f"[Trace] Patching static property: {name_dot(cls,property_name)}")
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
        result = self.observer.notify(
            None, 'get', owner, self.name, current_value, level='class'
        )
        if isinstance(result, dict) and result.get('do_not_really_get', False):
            return result.get('return_value')
        return current_value

    def __set__(self, instance, new_value):
        result = self.observer.notify(
            None, 'set', instance, self.name, new_value, level='class'
        )
        if isinstance(result, dict) and result.get('do_not_really_set', False):
            return
        if isinstance(result, dict) and result.get('really_set', False):
            self.value = result.get('new_value')
            return
        self.value = new_value

# 4. Inspector Class
class Inspector:
    def __init__(self):
        self.printed_classes = set()
        self.printed_members = set()
        self.class_hierarchy = {}
        self.captured_members = {}
        self.patched_members = {}

        self.observer = Observer()
        self.monkey_patcher = MonkeyPatcher(self.observer)

    def mark_class(self, obj, no_filter=False):
        if not inspect.isclass(obj):
            mesg(f"Warning: {obj} is not a class. Skipping.")
            return

        obj_name = obj.__name__
        if obj_name in self.printed_classes:
            return

        mesg(f"\nInspecting {obj_name}:")
        self.get_members(obj, no_filter)
        self.printed_classes.add(obj_name)
        self.class_hierarchy[obj_name] = [base for base in obj.__bases__ if base is not object]

    def get_members(self, obj, no_filter=False):
        obj_name = obj.__name__
        members = inspect.getmembers(obj)
        member_list = []

        for name, member in members:
            if name.startswith('_') and not (name.startswith('__') and name.endswith('__')):
                continue

            implemented_from = implemented_in(name, obj)
            is_callable = callable(member)
            self_usage = uses_self(member) if is_callable else False

            member_type = classify_member_type(obj, name, member)
            member_level = classify_member_level(obj, name, member, implemented_from, self_usage)
            implementation = classify_implementation(obj, implemented_from)

            if not no_filter:
                if implementation != Implemented.LOCAL.value:
                    continue
                if member_level == MemberLevel.CLASS and member_type == MemberType.SPECIAL_VARIABLE:
                    continue

            if (obj_name, name) in self.printed_members:
                continue
            self.printed_members.add((obj_name, name))

            member_info = {
                'name': name,
                'member': member,
                'member_type': member_type,
                'level': member_level,
                'implemented': implementation,
                'implemented_from': implemented_from,
                'class_name': obj_name,
                'class_object': obj
            }
            member_list.append(member_info)

        if obj_name not in self.captured_members:
            self.captured_members[obj_name] = []
        self.captured_members[obj_name].extend(member_list)

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

    def mark_base_classes(self, no_filter=False):
        for class_name in list(self.class_hierarchy.keys()):
            bases = self.class_hierarchy[class_name]
            for base in bases:
                base_name = base.__name__
                if base_name not in self.printed_classes:
                    mesg(f"\nInspecting base class {base_name}:")
                    self.mark_class(base, no_filter=no_filter)

    def monkey_patch_members(self):
        """Monkey-patch captured members using the MonkeyPatcher."""
        for class_name, members in self.captured_members.items():
            cls = members[0]['class_object']  # All members belong to the same class
            for member_info in members:

                name = member_info['name']
                member = member_info['member']
                level = member_info['level']
                member_type = member_info['member_type']

                qualified_name = f"{name_dot(cls,name)}"
                if callable(member):
                    qualified_name = f"{qualified_name}{signature(member)}"

                if not member in self.patched_members:
                    self.patched_members[member] = member_info
                else:
                    continue

                try:
                    if member_type == MemberType.METHOD:
                        if level == MemberLevel.INSTANCE:
                            mesg(f"Patching instance method: {qualified_name}")
                            self.monkey_patcher.patch_instance_method(cls, name, member)
                        elif level == MemberLevel.CLASS:
                            mesg(f"Patching static class method: {qualified_name}")
                            self.monkey_patcher.patch_class_method(cls, name, member)
                        else:
                            mesg(f"Skipping method {qualified_name} with unknown level: {level}")
                    elif member_type == MemberType.CLASS_METHOD:
                        mesg(f"Patching (static) class method: {qualified_name}")
                        self.monkey_patcher.patch_class_method(cls, name, member)
                    elif member_type == MemberType.STATIC_METHOD:
                        mesg(f"Patching static method: {qualified_name}")
                        self.monkey_patcher.patch_static_method(cls, name, member)
                    elif member_type == MemberType.PROPERTY:
                        mesg(f"Patching property: {qualified_name}")
                        # Retrieve the original property descriptor
                        original_property = get_member_descriptor(cls, name)
                        self.monkey_patcher.patch_instance_property(cls, name, original_property, level='instance')
                    elif member_type == MemberType.VARIABLE:
                        if level == MemberLevel.CLASS:
                            mesg(f"Patching static class variable: {qualified_name}")
                            self.monkey_patcher.patch_static_property(cls, name, member)
                        elif level == MemberLevel.INSTANCE:
                            mesg(f"Patching instance variable: {qualified_name}")
                            self.monkey_patcher.patch_instance_property(cls, name, member, level='instance')
                    elif member_type == MemberType.FUNCTION:
                        # Functions inside classes are treated as methods
                        if level == MemberLevel.INSTANCE:
                            mesg(f"Patching instance function: {qualified_name}")
                            self.monkey_patcher.patch_instance_method(cls, name, member)
                        else:
                            mesg(f"Patching static class function: {qualified_name}")
                            self.monkey_patcher.patch_static_method(cls, name, member)
                    elif member_type == MemberType.SPECIAL_METHOD:
                        if level == MemberLevel.INSTANCE:
                            mesg(f"Patching special instance method: {qualified_name}")
                            self.monkey_patcher.patch_instance_method(cls, name, member)
                        elif level == MemberLevel.CLASS:
                            mesg(f"Patching special class method: {qualified_name}")
                            self.monkey_patcher.patch_class_method(cls, name, member)
                        else:
                            mesg(f"Skipping special method {qualified_name} with unknown level: {level}")
                    else:
                        mesg(f"Skipping unsupported member type: {member_type} for {qualified_name}")
                except Exception as e:
                    mesg(f"Error patching {qualified_name}: {e}")

    def monkey_patch_module(self, module):
        """Monkey-patch functions and variables in a module."""
        for name, obj in inspect.getmembers(module):
            if name.startswith('_'):
                continue

            try:
                if inspect.isfunction(obj) or inspect.isbuiltin(obj):
                    #mesg(f"Patching module function: {name_dot(module, name)}")
                    self.monkey_patcher.patch_module_function(module, name, obj)
                elif not callable(obj):
                    mesg(f"Borked Patching of module variable: {name_dot(module, name)}")
                    self.monkey_patcher.patch_module_property(module, name, obj)
            except Exception as e:
                mesg(f"Error patching {name_dot(module, name)}: {e}")



# Helper functions
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

    def __repr__(self):
        return "inst_repr"

    def __init__(self):
        super().__init__()  # Call base class __init__ to inherit base instance variables
        self.instance_var = 10  # Instance-level attribute

    @staticmethod
    def static_method():
        pass

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

# Main execution
if __name__ == "__main__":
    inspector = Inspector()

    # Subscribe to events
    def before_call_callback(*args):
        target_class, method_name = args[0], args[1]
        mesg(f"[Observer] Before calling {name_dot(target_class, method_name)}{args[2:]}")
        # Do not return anything unless modifying args and kwargs

    def after_call_callback(*args):
        target_class, method_name, result = args[0], args[1], args[2]
        mesg(f"[Observer] After calling {name_dot(target_class, method_name)} -> {result}")
        # Do not return anything unless modifying the result

    def get_callback(*args):
        target_class, property_name, current_value = args[0], args[1], args[2]
        mesg(f"[Observer] Getting {name_dot(target_class, property_name)}, current value: {current_value}")
        # Do not return anything unless modifying the value

    def set_callback(*args):
        target_class, property_name, new_value = args[0], args[1], args[2]
        mesg(f"[Observer] Setting {name_dot(target_class, property_name)} to {new_value}")
        # Do not return anything unless modifying the value

    # Subscribe to observer events
    inspector.observer.subscribe('before_call', before_call_callback, level='instance')
    inspector.observer.subscribe('after_call', after_call_callback, level='instance')
    inspector.observer.subscribe('before_call', before_call_callback, level='class')
    inspector.observer.subscribe('after_call', after_call_callback, level='class')
    inspector.observer.subscribe('before_call', before_call_callback, level='module')
    inspector.observer.subscribe('after_call', after_call_callback, level='module')

    inspector.observer.subscribe('get', get_callback, level='instance')
    inspector.observer.subscribe('set', set_callback, level='instance')
    inspector.observer.subscribe('get', get_callback, level='class')
    inspector.observer.subscribe('set', set_callback, level='class')
    inspector.observer.subscribe('get', get_callback, level='module')
    inspector.observer.subscribe('set', set_callback, level='module')

    mesg("\nInspecting hyperon:")
    import hyperonpy
    from hyperon.atoms import *  # Import your classes
    from hyperon.runner import MeTTa  # Import your classes
    inspector.monkey_patch_module(hyperonpy)
    inspector.monkey_patch_members()

    mesg("\nInspecting MeTTa classes:")
    
    # next two lines will cause several callback in hyperonpy
    metta = MeTTa()
    print(metta.run("!(+ 1 1)"))

    print("CLOSE_PAREN=",hyperonpy.CLOSE_PAREN)

    input("\nPress Enter to inspect MyClass with filters...")

    mesg("Inspecting MyClass with filters:")
    inspector.mark_class(MyClass)
    
    inspector.mark_base_classes()

    # Monkey-patch captured members
    inspector.monkey_patch_members()

    # Test the monkey-patched methods
    mesg("\nTesting monkey-patched methods:")
    obj = MyClass()
    obj.instance_method()
    obj.__repr__()
    MyClass.class_method_without_self()
    MyClass.class_method_with_self(obj)  # Provide 'self' as argument if required
    MyClass.static_method()
    obj.my_instance_property  # This should trigger the get_callback
    obj.my_instance_property = 200       # This will trigger the set_callback
    MyClass.class_var
    # Test the monkey-patched static properties and methods
    mesg("\nTesting monkey-patched static class property:")
    MyClass.class_var = 300  # This will trigger the set_callback for class_var


    input("\nPress Enter to inspect Atoms...")

    # Re-instantiate the Inspector to reset the state
    #inspector = Inspector()

    # Inspect classes with no_filter=True to include all members
    inspector.mark_class(GroundedAtom)
    inspector.mark_class(VariableAtom)
    inspector.mark_class(SymbolAtom)
    inspector.mark_class(ExpressionAtom)
    inspector.mark_class(ValueObject)
    inspector.mark_class(OperationObject)
    inspector.mark_class(MatchableObject)
    # inspector.mark_class(Bindings)
    # inspector.mark_class(BindingsSet)
    # inspector.mark_class(Atom)

    inspector.mark_base_classes(no_filter=False)

    # Monkey-patch captured members
    inspector.monkey_patch_members()

    # Subscribe to events for Atom classes
    #inspector.observer.subscribe('before_call', before_call_callback, level='instance')
    #inspector.observer.subscribe('after_call', after_call_callback, level='instance')
    #inspector.observer.subscribe('get', get_callback)
    #inspector.observer.subscribe('set', set_callback)

    # Example usage (replace with actual method calls)
    mesg("\nTesting monkey-patched Atom methods:")
    atom = ValueObject("I am a String")
    # no setter atom.value = "I am a lower string"
    atom.value
    print(str(atom))
    print(repr(atom))

