import sys
import numbers

try:
    from io import StringIO # Python 3
except:
    from StringIO import StringIO


# Direct stdout to a StringIO buffer,
# to prevent commands from printing to the output stream

write_stream = sys.stdout
redirect_stream = StringIO()

sys.stdout = redirect_stream

class Symbol:
    """
    A wrapper around a string, representing a Lisp symbol. 
    """
    def __init__(self, name):
        self._name = name
    def __str__(self):
        return self._name
    def __repr__(self):
        return "Symbol("+self._name+")"

##################################################################
# This code adapted from cl4py
#
# https://github.com/marcoheisig/cl4py
#
# Copyright (c) 2018  Marco Heisig <marco.heisig@fau.de>

def lispify(obj):
    return lispify_aux(obj)

def lispify_aux(obj):
    try:
        return lispifiers[type(obj)](obj)
    except KeyError:
        # Special handling for numbers. This should catch NumPy types
        # as well as built-in numeric types
        if isinstance(obj, numbers.Number):
            return str(obj)
        
        # Another unknown type
        return "NIL"

lispifiers = {
    bool       : lambda x: "T" if x else "NIL",
    type(None) : lambda x: "NIL",
    int        : lambda x: str(x),
    float      : lambda x: str(x),
    complex    : lambda x: "#C(" + lispify_aux(x.real) + " " + lispify_aux(x.imag) + ")",
    list       : lambda x: "#(" + " ".join(lispify_aux(elt) for elt in x) + ")",
    tuple      : lambda x: "(" + " ".join(lispify_aux(elt) for elt in x) + ")",
    dict       : lambda x: "#.(let ((table (make-hash-table))) " + " ".join("(setf (gethash {} table) {})".format(key, value) for key, value in x.items()) + " table)",
    str        : lambda x: "\"" + x.replace("\\", "\\\\").replace('"', '\\"')  + "\"",
    Symbol     : lambda x: str(x)
}

##################################################################

eval_globals = {}
eval_locals = {}

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
    value_str = lispify(value)
    print(len(value_str))
    write_stream.write(value_str)
    write_stream.flush()

def return_value(value):
    """
    Send a value to stdout
    """
    # Mark response as a returned value
    try:
        sys.stdout = write_stream
        write_stream.write("r")
        send_value(value)
    finally:
        sys.stdout = redirect_stream

def return_error(err):
    """
    Send an error message
    """
    try:
        sys.stdout = write_stream
        write_stream.write("e")
        send_value(str(err))
    finally:
        sys.stdout = redirect_stream

def message_dispatch_loop():
    """
    Wait for a message, dispatch on the type of message
    """
    while True:
        try:
            # Read command type
            cmd_type = sys.stdin.read(1)
            
            if cmd_type == "e":  # Evaluate an expression
                result = eval(recv_string(), eval_globals, eval_locals)
                return_value(result)
        
            elif cmd_type == "x": # Execute a statement
                exec(recv_string(), eval_globals, eval_locals)
                return_value(None)
            
            elif cmd_type == "q": # Quit
                sys.exit(0)
                
            elif cmd_type == "r": # Return value from Lisp function
                return recv_value()

            elif cmd_type == "f": # Function call
                # Get a tuple (function, allargs)
                fn_name, allargs = recv_value()

                # Split positional arguments and keywords
                args = []
                kwargs = {}
                it = iter(allargs) # Use iterator so we can skip values
                for arg in it:
                    if isinstance(arg, Symbol):
                        # A keyword. Take the next value
                        kwargs[ str(arg)[1:] ] = next(it)
                        continue
                    args.append(arg)
                
                # Get the function object. Using eval to handle cases like "math.sqrt" or lambda functions
                function = eval(fn_name, eval_globals, eval_locals)
                return_value( function(*args, **kwargs) )
                
            else:
                return_error("Unknown message type '{0}', content: {1}".format(cmd_type, cmd_string))
            
        except Exception as e:
            return_error(e)

        
def callback_func(ident, *args, **kwargs):
    """
    Call back to Lisp

    ident  Uniquely identifies the function to call
    args   Arguments to be passed to the function
    """

    # Convert kwargs into a sequence of ":keyword value" pairs
    # appended to the positional arguments
    allargs = args
    for key, value in kwargs.items():
        allargs += (Symbol(":"+str(key)), value)
    
    try:
        sys.stdout = write_stream
        write_stream.write("c")
        send_value((ident, allargs))
    finally:
        sys.stdout = redirect_stream

    # Wait for a value to be returned.
    # Note that the lisp function may call python before returning
    return message_dispatch_loop()

# Make callback function accessible to evaluation
eval_globals["_py4cl_callback"] = callback_func
eval_globals["_py4cl_Symbol"] = Symbol

# Main loop
message_dispatch_loop()



