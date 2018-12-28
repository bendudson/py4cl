
import sys

eval_globals = {}
eval_locals = {}

def send_value(value):
    """
    Send a value to stdout as a string, with length of string first
    """
    value_str = str(value)
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

def return_error(value):
    """
    Send an error message
    """
    sys.stdout.write("e")
    send_value(value)
    
while True:
    try:
        # Read command header
        header = sys.stdin.readline()
        if len(header) == 0:
            continue
        cmd_type = header[0]  # First character specifies type of command
        cmd_length = int(header[1:]) # Remainder is the length
        cmd_string = sys.stdin.read(cmd_length)
        
        if cmd_type == "e":
            return_value(eval(cmd_string, eval_globals, eval_locals))
        else:
            exec(cmd_string, eval_globals, eval_locals)
            return_value(None)
    except Exception as e:
        return_error(e)


