# Python interface for py4cl
# 
# This code handles messages from lisp, marshals and unmarshals data,
# and defines classes which forward all interactions to lisp.
#
# Should work with python 2.7 or python 3

from __future__ import print_function

import sys
import numbers
import itertools
import inspect

try:
    from io import StringIO # Python 3
except:
    from io import BytesIO as StringIO

# Direct stdout to a StringIO buffer,
# to prevent commands from printing to the output stream

write_stream = sys.stdout
redirect_stream = StringIO()

sys.stdout = redirect_stream

class Symbol(object):
    """
    A wrapper around a string, representing a Lisp symbol. 
    """
    def __init__(self, name):
        self._name = name
    def __str__(self):
        return self._name
    def __repr__(self):
        return "Symbol("+self._name+")"

class LispCallbackObject (object):
    """
    Represents a lisp function which can be called. 
    
    An object is used rather than a lambda, so that the lifetime
    can be monitoried, and the function removed from a hash map
    """
    def __init__(self, handle):
        """
        handle    A number, used to refer to the object in Lisp
        """
        self.handle = handle

    def __del__(self):
        """
        Delete this object, sending a message to Lisp
        """
        try:
            sys.stdout = write_stream
            write_stream.write("d")
            send_value(self.handle)
        finally:
            sys.stdout = redirect_stream

    def __call__(self, *args, **kwargs):
        """
        Call back to Lisp
        
        args   Arguments to be passed to the function
        """
        global return_values
        
        # Convert kwargs into a sequence of ":keyword value" pairs
        # appended to the positional arguments
        allargs = args
        for key, value in kwargs.items():
            allargs += (Symbol(":"+str(key)), value)

        old_return_values = return_values # Save to restore after
        try:
            return_values = 0 # Need to send the values
            sys.stdout = write_stream
            write_stream.write("c")
            send_value((self.handle, allargs))
        finally:
            return_values = old_return_values
            sys.stdout = redirect_stream

        # Wait for a value to be returned.
        # Note that the lisp function may call python before returning
        return message_dispatch_loop()

    
class UnknownLispObject (object):
    """
    Represents an object in Lisp, which could not be converted to Python
    """
    def __init__(self, lisptype, handle):
        """
        lisptype  A string describing the type. Mainly for debugging
        handle    A number, used to refer to the object in Lisp
        """
        self.lisptype = lisptype
        self.handle = handle

    def __del__(self):
        """
        Delete this object, sending a message to Lisp
        """
        try:
            sys.stdout = write_stream
            write_stream.write("d")
            send_value(self.handle)
        finally:
            sys.stdout = redirect_stream

    def __str__(self):
        return "UnknownLispObject(\""+self.lisptype+"\", "+str(self.handle)+")"

    def __getattr__(self, attr):
        # Check if there is a slot with this name
        try:
            sys.stdout = write_stream
            write_stream.write("s") # Slot access
            send_value((self.handle, attr))
        finally:
            sys.stdout = redirect_stream

        # Wait for the result
        return message_dispatch_loop()
        
# These store the environment used when eval'ing strings from Lisp
eval_globals = {}
eval_locals = {}

# Settings

return_values = 0 # Try to return values to lisp. If > 0, always return a handle
                  # A counter is used, rather than Boolean, to allow nested environments.

python_to_lisp_type = {
  bool: "BOOLEAN",
  type(None): "NULL",
  int: "INTEGER",
  float: "FLOAT",
  complex: "COMPLEX",
  list: "VECTOR",
  dict: "HASH-TABLE",
  str: "STRING",
  inspect._empty: "NIL"
}
    
##################################################################
# This code adapted from cl4py
#
# https://github.com/marcoheisig/cl4py
#
# Copyright (c) 2018  Marco Heisig <marco.heisig@fau.de>
#               2019  Ben Dudson <benjamin.dudson@york.ac.uk>

lispifiers = {
    bool       : lambda x: "T" if x else "NIL",
    type(None) : lambda x: "NIL",
    int        : str,
    float      : str,
    complex    : lambda x: "#C(" + lispify(x.real) + " " + lispify(x.imag) + ")",
    list       : lambda x: "#(" + " ".join(lispify(elt) for elt in x) + ")",
    tuple      : lambda x: "(" + " ".join(lispify(elt) for elt in x) + ")",
    # Note: With dict -> hash table, use :test 'equal so that string keys work as expected
    dict       : lambda x: "#.(let ((table (make-hash-table :test 'equal))) " + " ".join("(setf (gethash {} table) {})".format(lispify(key), lispify(value)) for key, value in x.items()) + " table)",
    str        : lambda x: "\"" + x.replace("\\", "\\\\").replace('"', '\\"')  + "\"",
    type       : lambda x: python_to_lisp_type[x],
    Symbol     : str,
    UnknownLispObject : lambda x: "#.(py4cl::lisp-object {})".format(x.handle),
}

# This is used to test if a value is a numeric type
numeric_base_classes = (numbers.Number,)

try:
    # Use NumPy for multi-dimensional arrays
    import numpy

    def lispify_ndarray(obj):
        """Convert a NumPy array to a string which can be read by lisp
        Example:
        array([[1, 2],     => '#2A((1 2) (3 4))'
              [3, 4]])
        """
        if obj.ndim == 0:
            # Convert to scalar then lispify
            return lispify(numpy.asscalar(obj))
        
        def nested(obj):
            """Turns an array into nested ((1 2) (3 4))"""
            if obj.ndim == 1: 
                return "("+" ".join([lispify(i) for i in obj])+")" 
            return "(" + " ".join([nested(obj[i,...]) for i in range(obj.shape[0])]) + ")"

        return "#{:d}A".format(obj.ndim) + nested(obj)

    # Register the handler to convert Python -> Lisp strings
    lispifiers[numpy.ndarray] = lispify_ndarray

    # NumPy is used for Lisp -> Python conversion of multidimensional arrays
    eval_globals["_py4cl_numpy"] = numpy

    # Register numeric base class
    numeric_base_classes += (numpy.number,)
except:
    pass

def lispify_handle(obj):
    """
    Store an object in a dictionary, and return a handle
    """
    handle = next(python_handle)
    python_objects[handle] = obj
    return "#.(py4cl::make-python-object-finalize :type \""+str(type(obj))+"\" :handle "+str(handle)+")"

def lispify(obj):
    """
    Turn a python object into a string which can be parsed by Lisp's reader.
    
    If return_values is false then always creates a handle
    """
    if return_values > 0:
        return lispify_handle(obj)

    try:
        return lispifiers[type(obj)](obj)
    except KeyError:
        # Special handling for numbers. This should catch NumPy types
        # as well as built-in numeric types
        if isinstance(obj, numeric_base_classes):
            return str(obj)
        
        # Another unknown type. Return a handle to a python object
        return lispify_handle(obj)


##################################################################

def recv_string():
    """
    Get a string from the input stream
    """
    # First a line containing the length as a string
    length = int(sys.stdin.readline())
    # Then the specified number of bytes
    return sys.stdin.read(length)

def recv_value():
    """
    Get a value from the input stream
    Return could be any type
    """
    return eval(recv_string(), eval_globals, eval_locals)

def send_value(value):
    """
    Send a value to stdout as a string, with length of string first
    """
    try:
        value_str = lispify(value)
    except Exception as e:
        # At this point the message type has been sent,
        # so we can't change to throw an exception/signal condition
        value_str = "Lispify error: " + str(e)
    print(len(value_str))
    write_stream.write(value_str)
    write_stream.flush()

def return_stdout():
    """
    Return the contents of redirect_stream, to be printed to stdout
    """
    global redirect_stream
    global return_values
    
    contents = redirect_stream.getvalue()
    if not contents:
        return  # Nothing to send

    redirect_stream = StringIO() # New stream, delete old one

    old_return_values = return_values # Save to restore after
    try:
        return_values = 0 # Need to return the string, not a handle
        sys.stdout = write_stream
        write_stream.write("p")
        send_value(contents)
    finally:
        return_values = old_return_values
        sys.stdout = redirect_stream
    
def return_error(err):
    """
    Send an error message
    """
    global return_values

    return_stdout() # Send stdout if any
    
    old_return_values = return_values # Save to restore after
    try:
        return_values = 0 # Need to return the error, not a handle
        sys.stdout = write_stream
        write_stream.write("e")
        send_value(str(err))
    finally:
        return_values = old_return_values
        sys.stdout = redirect_stream

def return_value(value):
    """
    Send a value to stdout
    """
    if isinstance(value, Exception):
        return return_error(value)

    return_stdout() # Send stdout if any
    
    # Mark response as a returned value
    try:
        sys.stdout = write_stream
        write_stream.write("r")
        send_value(value)
    finally:
        sys.stdout = redirect_stream

def pythonize(value):
    """
    Convertes the value (Symbol) to python conventioned strings.
    In particular, replaces "-" with "_"
    """
    return str(value)[1:].replace('-', '_')
        
def message_dispatch_loop():
    """
    Wait for a message, dispatch on the type of message.
    Message types are determined by the first character:

    e  Evaluate an expression (expects string)
    x  Execute a statement (expects string)
    q  Quit
    r  Return value from lisp (expects value)
    f  Function call
    a  Asynchronous function call
    R  Retrieve value from asynchronous call
    s  Set variable(s) 
    """
    global return_values  # Controls whether values or handles are returned
    
    while True:
        try:
            # Read command type
            cmd_type = sys.stdin.read(1)
            
            if cmd_type == "e":  # Evaluate an expression
                result = eval(recv_string(), eval_globals, eval_locals)
                return_value(result)
            
            elif cmd_type == "f" or cmd_type == "a": # Function call
                # Get a tuple (function, allargs)
                fn_name, allargs = recv_value()

                # Split positional arguments and keywords
                args = []
                kwargs = {}
                if allargs:
                    it = iter(allargs) # Use iterator so we can skip values
                    for arg in it:
                        if isinstance(arg, Symbol):
                            # A keyword. Take the next value
                            kwargs[ pythonize(arg) ] = next(it)
                            continue
                        args.append(arg)
                
                # Get the function object. Using eval to handle cases like "math.sqrt" or lambda functions
                if callable(fn_name):
                    function = fn_name # Already callable
                else:
                    function = eval(fn_name, eval_globals, eval_locals)
                if cmd_type == "f":
                    # Run function then return value
                    return_value( function(*args, **kwargs) )
                else:
                    # Asynchronous

                    # Get a handle, and send back to caller.
                    # The handle can be used to fetch
                    # the result using an "R" message.
                    
                    handle = next(async_handle)
                    return_value(handle)

                    try:
                        # Run function, store result
                        async_results[handle] = function(*args, **kwargs)
                    except Exception as e:
                        # Catching error here so it can
                        # be stored as the return value
                        async_results[handle] = e
    
            elif cmd_type == "O":  # Return only handles
                return_values += 1

            elif cmd_type == "o":  # Return values when possible (default)
                return_values -= 1
                
            elif cmd_type == "q": # Quit
                sys.exit(0)
                
            elif cmd_type == "R":
                # Request value using handle
                handle = recv_value()
                return_value( async_results.pop(handle) )
                
            elif cmd_type == "r": # Return value from Lisp function
                return recv_value()
            
            elif cmd_type == "s":
                # Set variables. Should have the form
                # ( ("var1" value1) ("var2" value2) ...)
                setlist = recv_value()
                for name, value in setlist:
                    eval_locals[name] = value
                # Need to send something back to acknowlege
                return_value(True)

            elif cmd_type == "v":
                # Version info
                return_value(tuple(sys.version_info))
                
            elif cmd_type == "x": # Execute a statement
                exec(recv_string(), eval_globals, eval_locals)
                return_value(None)
                
            else:
                return_error("Unknown message type '{0}'".format(cmd_type))

        except Exception as e:
            return_error(e)


# Store for python objects which can't be translated to Lisp objects
python_objects = {}
python_handle = itertools.count(0) # Running counter

# Make callback function accessible to evaluation
eval_globals["_py4cl_LispCallbackObject"] = LispCallbackObject
eval_globals["_py4cl_Symbol"] = Symbol
eval_globals["_py4cl_UnknownLispObject"] = UnknownLispObject
eval_globals["_py4cl_objects"] = python_objects

async_results = {}  # Store for function results. Might be Exception
async_handle = itertools.count(0) # Running counter

# Main loop
message_dispatch_loop()



