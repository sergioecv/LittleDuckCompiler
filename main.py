from parser import Parser
from virtualMachine import VirtualMachine

def readProgram(file_path):
    # Open the file
    with open(file_path, 'r') as file:
        # Read the contents
        docstring = file.read()
    # Close the file
    return docstring

# Load program
file_path = 'textEditor.txt'
docstring = readProgram(file_path)

# Create parser
parser = Parser()
# Parse program
parser.parse_code(docstring)

# Create Virtual Machine
vm = VirtualMachine()

# Read obj.txt
vm.readFile('obj.txt')

# Map memory from file read
vm.createMapMemory()

# Execute code
vm.runCode()