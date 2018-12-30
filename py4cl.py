import sys

try:
    from io import StringIO # Python 3
except:
    from StringIO import StringIO


##################################################################
# This code adapted from cl4py
#
# https://github.com/marcoheisig/cl4py
#
# Copyright (c) 2018  Marco Heisig <marco.heisig@fau.de>

def lispify(obj):
    return lispify_aux(obj)

def lispify_aux(obj):
    return lispifiers[type(obj)](obj)

lispifiers = {
    bool       : lambda x: "T" if x else "NIL",
    type(None) : lambda x: "NIL",
    int        : lambda x: str(x),
    float      : lambda x: str(x),
    complex    : lambda x: "#C(" + lispify_aux(x.real) + " " + lispify_aux(x.imag) + ")",
    list       : lambda x: "#(" + " ".join(lispify_aux(elt) for elt in x) + ")",
    tuple      : lambda x: "(" + " ".join(lispify_aux(elt) for elt in x) + ")",
    dict       : lambda x: "#.(let ((table (make-hash-table))) " + " ".join("(setf (gethash {} table) {})".format(key, value) for key, value in x.items()) + " table)",
    str        : lambda x: "\"" + x + "\""
}

##################################################################

eval_globals = {}
eval_locals = {}

def send_value(value):
    """
    Send a value to stdout as a string, with length of string first
    """
    value_str = lispify(value)
    print(len(value_str))
    sys.stdout.write(value_str)
    sys.stdout.flush()

def return_value(value):
    """
    Send a value to stdout
    """
    # Mark response as a returned value
    sys.stdout.write("r")
    send_value(value)

def return_error(err):
    """
    Send an error message
    """
    sys.stdout.write("e")
    send_value(str(err))

# Main loop
while True:
    try:
        # Read command header
        header = sys.stdin.readline()
        if len(header) == 0:
            continue
        cmd_type = header[0]  # First character specifies type of command
        cmd_length = int(header[1:]) # Remainder is the length
        cmd_string = sys.stdin.read(cmd_length)
        
        if cmd_type == "e":  # Evaluate an expression
            # Temporarily direct stdout to a StringIO buffer,
            # to prevent commands from printing to the output stream
            oldstdout = sys.stdout
            sys.stdout = StringIO()
            try:
                result = eval(cmd_string, eval_globals, eval_locals)
            finally:
                sys.stdout = oldstdout # Restore
            return_value(result)
        
        elif cmd_type == "x": # Execute a statement
            oldstdout = sys.stdout
            sys.stdout = StringIO()
            try:
                exec(cmd_string, eval_globals, eval_locals)
            finally:
                sys.stdout = oldstdout
            return_value(None)
            
        elif cmd_type == "q": # Quit
            sys.exit(0)
            
    except Exception as e:
        return_error(e)


