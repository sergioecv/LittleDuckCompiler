import ply.yacc as yacc
import json
# Obtener tokens del scanner
from lex import tokens

from collections import deque


dirFunc = {}
varTable = {}

funcStack = deque()
varStack = deque()

# Quadruples stacks
quadruples = []
operatorStack = deque()
operandStack = deque()
typeStack = deque()
jumpStack = deque()
countMemQuad = 1

def result_type(operand1, operand2, operator):
    semantic_cube = {
        ('int', 'int', "+"): 'int',
        ('int', 'int', "-"): 'int',
        ('int', 'int', "*"): 'int',
        ('int', 'int', "/"): 'int',
        ('int', 'int', ">"): 'bool',
        ('int', 'int', "<"): 'bool',
        ('int', 'int', "!="): 'bool',

        ('int', 'float', "+"): 'float',
        ('int', 'float', "-"): 'float',
        ('int', 'float', "*"): 'float',
        ('int', 'float', "/"): 'float',
        ('int', 'float', ">"): 'bool',
        ('int', 'float', "<"): 'bool',
        ('int', 'float', "!="): 'bool',

        ('float', 'float', "+"): 'float',
        ('float', 'float', "-"): 'float',
        ('float', 'float', "*"): 'float',
        ('float', 'float', "/"): 'float',
        ('float', 'float', ">"): 'bool',
        ('float', 'float', "<"): 'bool',
        ('float', 'float', "!="): 'bool'
    }

    key = (operand1, operand2, operator)
    key_alt = (operand2, operand1, operator)
    error_message = "error"

    return semantic_cube.get(
        key, semantic_cube.get(
            key_alt, error_message
            ))

#                                         <PROGRAMA>     
def p_programa(p):
    # Body
    'programa : PROGRAM ID SEMICOLON dec_vars gl_vars dec_funcs MAIN body END'
    p[0] = ('program', p[2], p[4], p[5], ('main', p[8]))

def p_gl_vars(p):
    'gl_vars : '
    # Declare global variables
    name_global = ('global', 'void')
    dirFunc[name_global] = varTable.copy()
    varTable.clear()
    

def p_dec_vars(p):
    '''dec_vars : vars
                | empty
    '''
    p[0] = p[1]

def p_dec_funcs(p):
    '''dec_funcs : funcs dec_funcs
                | empty
    '''
    
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        global varTable
        varTable = dirFunc[('global', 'void')].copy()

#                                         <VARS>     
def p_vars(p):
    '''vars : VAR variables
    '''
    variable, tp, variables = p[2]
    p[0] = ('var', variable, tp, variables)
    # if variables == None:
    #     p[0] = ('var', variable, tp)
    # else:
    #     p[0] = ('var', variable, tp, variables)

    # if len(funcStack) == 0:
        # name_global = ('global', 'void')
        # dirFunc[name_global] = varTable.copy()
        # varTable.clear()

def p_variables(p):
    '''variables : list_ids COLON type SEMICOLON variables
                 | empty
    '''
    if len(p) != 2:
        p[0] = [p[1], p[3], p[5]]
        

def p_list_ids(p):
    'list_ids : ID more_ids'
    name_id = p[1]
    if p[2] == None:
        p[0] = [name_id]
    else:
        p[0] = [name_id , p[2]]
    varStack.append(name_id)

def p_more_ids(p):
    '''
    more_ids : COMMA list_ids
             | empty
    '''
    if len(p) != 2:
        p[0] = p[2]

# Insert variables to varTable
def var_insert(var_type):

    while (len(varStack) != 0):
        name_id = varStack.pop()

        # Validate id declaration
        if name_id not in varTable:
            # Insert variable
            varTable[name_id] = var_type
        else:
            # Error
            raise ReferenceError(f"Multiple declaration: '{name_id}'") 
        typeStack.clear()

#                                         <TYPE>     
def p_type(p):
    '''
    type : INT
         | FLOAT
    '''
    p[0] = p[1]
    _type = p[1]
    # typeStack.append(_type)
    if len(varStack) > 0:
        var_insert(_type)

#                                         <BODY>   
def p_body(p):
    '''
    body : LBRACE statement_list RBRACE
    '''
    p[0] = p[2]

def p_statement_list(p):
    '''
    statement_list : statement statement_list
         | empty
    '''
    if len(p) == 3:
        print('STATEMENT', [p[1],p[2]])
        if p[2] == None:
            p[0] = p[1]
        else:
            p[0] = [p[1],p[2]]
    

#                                         <STATEMENT>   
def p_statement(p):
    '''
    statement : assign
              | condition
              | cycle
              | f_call
              | print
    '''
    p[0] = p[1]

#                                         <PRINT>   
def p_print(p):
    '''
    print : PRINT LPAREN dec_print RPAREN SEMICOLON
    '''
    p[0] = (p[1], p[3])

def p_dec_print(p):
    '''
    dec_print : string_exp print_exp more_print
              | expresion print_exp more_print 
    '''
    if p[3] == None:
        p[0] = p[1]
    else:
        p[0] = [p[1] , p[3]]

def p_string_exp(p):
    '''
    string_exp : STRING 
    '''
    string_exp = p[1]
    p[0] = string_exp
    operandStack.append(string_exp)

def p_print_exp(p):
    '''
    print_exp : 
    '''
    elem = operandStack.pop()
    quadruples.append(['print', None, None, elem])

def p_more_print(p):
    '''
    more_print : COMMA dec_print
               | empty
    '''
    if len(p) != 2:
        p[0] = p[2] 

# Look for variable in global or function scope
def lookup_var(name_id):
    globalTable = dirFunc.get( ('global', 'void'))

    print('tipo', type(name_id))
    if type(name_id) == int:
        return 'int'
    elif type(name_id) == float:
        return 'float'

    # If variable table is global, then look for type
    if varTable is globalTable:
        return current_table.get(name_id)
    # look for local and global type
    else:
        return varTable.get(name_id, globalTable.get(name_id))

#                                         <ASSIGN>   
def p_assign(p):
    '''
    assign : ID EQUALS expresion SEMICOLON
    '''
    name_id, operator = p[1], p[2]

    # Get type from expression
    expression_type = typeStack.pop()

    variable_type = lookup_var(name_id)

    # Check if variable is declared in local scope
    if variable_type is None:
        raise ReferenceError(f"Variable {name_id} not declared")
    # Check if variable and expression type are valid
    elif variable_type != expression_type:
        raise TypeError(f"Expected type: {variable_type} instead of {expression_type}")
    
    p[0] = ('assign', p[1], p[3])

    # Operand
    result_operand = operandStack.pop()
    # Assign quadruple
    quadruples.append([operator, result_operand, None, name_id])
#                                         <CYCLE>   
def p_cycle(p):
    '''
    cycle : do_cycle body WHILE LPAREN expresion RPAREN SEMICOLON
    '''
    p[0] = (p[1], p[2], p[3], [5])

    # Validate if expression
    exp_type = typeStack.pop()
    if(exp_type != 'bool'):
        raise TypeError(f"Type mismatch in expression: {exp_type}")
    else:
        # Result expression
        result = operandStack.pop()
        # Jump to beginning of cycle
        return_while = jumpStack.pop()
        # Add quadruple jump
        quadruples.append(['goToT', result, None, return_while])

def p_cycle_start(p):
    'do_cycle : DO'
    jumpStack.append(len(quadruples)+1)

#                                         <CONDITION>   
def p_condition(p):
    '''
    condition : init_condition body not_condition SEMICOLON
    '''
    p[0] = (p[1], p[2], p[3])

    # Get end of condition jump
    end = jumpStack.pop()
    # Save where the program continues after the condition
    quadruples[end][3] = len(quadruples)+1

def p_init_condition(p):
    '''
    init_condition : IF LPAREN expresion RPAREN
    '''
    p[0] = (p[1], p[3])
    # Validate if expression
    exp_type = typeStack.pop()
    if(exp_type != 'bool'):
        raise TypeError(f"Type mismatch in expression: {exp_type}")
    else:
        # Result expression
        result = operandStack.pop()
        # Add quadruple jump
        quadruples.append(['goToF', result, None, None])
        # Keep track of jump instruction
        jumpStack.append(len(quadruples)-1)

def p_not_condition(p):
    '''
    not_condition : ELSE define_jump body
              | empty
    '''
    if len(p) == 4:
        p[0] = [p[1] , p[3]]

def p_define_jump(p):
    '''
    define_jump : empty
    '''
     
    quadruples.append(['goTo', None, None, None])

    false = jumpStack.pop()
    jumpStack.append(len(quadruples)-1)

    quadruples[false][3] = len(quadruples)+1

#                                         <EXPRESSION>   
def p_expresion(p):
    '''
    expresion : exp compare_expresion
    '''
    op1 = p[1]
    if p[2] != None:
        comp_op, op2 = p[2]
        p[0] = (op1,comp_op, op2)
    else:
        p[0] = (op1)

def p_compare_expresion(p):
    '''
    compare_expresion : op_comp exp
                   | empty 
    '''
    if len(p) == 3:
        operator, operand = p[1], p[2]
        p[0] = [operator, operand]
        
        
#                                         <EXP>   
def p_exp(p):
    '''
    exp : termino add_sub
    '''
    op1 = p[1]
    if p[2] != None:
        operator, op2 = p[2]
        p[0] = op1 , operator, op2
    else:
        p[0] = p[1]
    solve_pending_operation(('<','>','!='))

def p_add_sub(p):
    '''
    add_sub : op_plus_min exp
        | empty
    '''
    if len(p) == 3:
        operator, operand = p[1], p[2]
        p[0] = [operator, operand]
        # operatorStack.append(operator)

def p_op_comp(p):
    '''
    op_comp : GREATER
             | LESS
             | NOT
    '''
    operator = p[1]
    p[0] = operator
    operatorStack.append(operator)

def p_op_plus_min(p):
    '''
    op_plus_min : PLUS
             | MINUS
    '''
    operator = p[1]
    p[0] = operator
    operatorStack.append(operator)

def p_op_times_divide(p):
    '''
    op_times_divide : TIMES
             | DIVIDE
    '''
    operator = p[1]
    p[0] = operator
    operatorStack.append(operator)

def solve_pending_operation(operators_opt):
    if operatorStack and (operatorStack[-1] in operators_opt):
        # Types
        right_type, left_type = typeStack.pop(), typeStack.pop()
        # Operands
        right_operand, left_operand = operandStack.pop(), operandStack.pop()
        # Operator
        oper = operatorStack.pop()
        global countMemQuad
        res_type = result_type(left_type, right_type, oper)
        if(res_type == 'error'):
            raise TypeError(f"Type mismatch in comparison: {left_type} {oper} {right_type}")
        else:
            operandStack.append('t'+ str(countMemQuad))
            typeStack.append(res_type)
            quadruples.append([oper, left_operand, right_operand, 't'+ str(countMemQuad)])
            countMemQuad += 1 


#                                         <TERMINO>   
def p_termino(p):
    '''
    termino : factor mult_div
    '''
    op1 = p[1]
    if p[2] != None:
        operator, op2 = p[2]
        p[0] = op1 , operator, op2
    else:
        p[0] = p[1]

    solve_pending_operation(('+','-'))

def p_mult_div(p):
    '''
    mult_div : op_times_divide termino
             | empty
    '''
    if len(p) == 3:
        operator, operand = p[1], p[2]
        p[0] = [operator, operand]
        # operatorStack.append(operator)
        print('yep', operatorStack)
#                                         <FACTOR>   
def p_factor(p):
    '''
    factor : LPAREN seen_lparen expresion RPAREN seen_rparen
             | MINUS dec_num
             | PLUS dec_num
             | dec_num
    '''
    print('factorthis', len(p))
    if len(p) == 6:
        p[0] = p[3]
    elif len(p) == 3:
        p[0] = [p[1], p[2]]
    else:
        p[0] = p[1]

    solve_pending_operation(('*','/'))

def p_seen_lparen(p):
    'seen_lparen :'
    operatorStack.append('(')


def p_seen_rparen(p):
    'seen_rparen :'
    operatorStack.reverse()
    operatorStack.remove('(')
    operatorStack.reverse()

def p_dec_num(p):
    '''
    dec_num : cte
            | ID
    '''
    name_id = p[1]
    p[0] = name_id

    if type(name_id) != int and type(name_id) != float:
        variable_type = lookup_var(name_id)

        if variable_type is None:
            raise ReferenceError(f"Variable {name_id} not declared")
        else:
            operandStack.append(name_id)
            typeStack.append(variable_type)

#                                         <CTE>   
def p_cte(p):
    '''
    cte : NUMBER
        | DECIMAL
    '''
    p[0] = p[1]
    cte = p[1]
    if type(cte) == int:
        typeStack.append('int')
    else:
        typeStack.append('float')
    print('this is cte', cte)
    operandStack.append(cte)
#                                         <FUNCS>   
def p_funcs(p):
    '''
    funcs : VOID ID LPAREN dec_params RPAREN LBRACK dec_vars body RBRACK SEMICOLON
    '''
    p[0] = (p[1], p[2], p[4], p[7], p[8])

    # Assign the name and type to dirFunc
    typeFunction, name = p[1], p[2]
    # Assign the table of variables
    dirFunc[(name, typeFunction)] = varTable.copy()
    # Clear the table of variables for new instances
    varTable.clear()

def p_dec_params(p):
    '''
    dec_params : ID COLON type more_params
               | empty
    '''
    if len(p) != 2:
        name_id, type_id = p[1], p[3]
        p[0] = [name_id, type_id, p[4]]
        varStack.append(name_id)
        var_insert(type_id)

def p_more_params(p):
    '''
    more_params : COMMA dec_params
               | empty
    '''
    if len(p) != 2:
        p[0] = p[2]

#                                         <F_CALL>   
def p_f_call(p):
    '''
    f_call : ID LPAREN dec_expresion RPAREN SEMICOLON
    '''
    # dec_expression
    name_id = p[1]
    if  name_id not in dirFunc:
        raise ReferenceError(f"Function {name_id} not declared")
    p[0] = ('f_call', p[1], p[3])

def p_dec_expresion(p):
    '''
    dec_expresion : expresion more_expresion
                  | empty
    '''
    if len(p) != 2:
        p[0] = [p[1], p[2]]

def p_more_expresion(p):
    '''
    more_expresion : COMMA dec_expresion
                  | empty
    '''
    if len(p) != 2:
        p[0] = p[2]

def p_empty(p):
    'empty :'
    pass

# Error rule for errors
def p_error(p):
    print("Error in parser")

programa1 = """
program prueba3;
var a, b, c, d: int;

main
{ 
    if( a + c > c * d){
        a = b + d;
    } else {
        a = d - c;
    };
    b = a * c + d;
}
end
"""
programa4 = """
program prueba2;
var a, b, c, d, e, f, g, h, i: int; j: float;



main
{
    if( a +c > c * d){
        a = b+d;
    
    };
    b = a * c + d;
    print("el resultado es", a, b+c*d);
}
end
"""

programa2 = """
program prueba2;

void one_param(variable1: int) [
    {
        a = a * (b - c * ( d + e) / f - g * h + i);
    }
];

main
{
    print("This is a print");
    one_param();

    do { a = b - c; } while ( a > 1 );
}
end
"""

programa3 = """
program prueba3;

var variable1, variable2: int; variable3: float;

void funcion(variable1: int, variable2: int) [
    var var_inside: int;
    {
        if (variable1 > variable2) {
            print(variable1);
        } else {
            print(j);
        };

        if (variable1 < variable2) {
        };
    }
];

main
{
    variable1 = 5;
    variable2 = 7 + 8;
    print("Value i=", i, " and j=", j);

    do { check = i; } while ( i > 1 );
}
end
"""

programa5 = """
program QuadTest;

var a, b, c, d, e, f: int;

main
{
    do {
        if (a + b < c) {
            a = b + c;
            do {
                a = a - 1;
            } while (a > b + c);
        } else {
            do {
                a = b + c * d;
                b = a - d;
            } while (b > c + d);
        };
    } while (a > b * c);
    a = b * c;
    c = 0;
}
end
"""

programa6 = """
program QuadTest;

var A, B, C, D, E, F, G, H, I, J, K: int;

main 
{
    A= B+C* (D-E/F)*H;
    B=E-F;
    do {

    } while (A*B-C > D*E/(G+H));

    H = J * K+B;
    if (B < H){
        B=H+J;
        print(A+B*C,D-E);
        }
    }
}


end
"""

# PARSE

parser = yacc.yacc(debug=True)

result = parser.parse(programa6,debug=True)

print(result)

print('         -------------        Quadruples     -------------')

for i, quadruple in enumerate(quadruples):
    print(f"{i+1}: [{quadruple[0]}, {quadruple[1]}, {quadruple[2]}, {quadruple[3]}]")


print('         -------------        varTable     -------------')

# print(varTable)
print(json.dumps(varTable, indent=4))

print('         -------------        dirFunc     -------------')
print(varTable)
# dirFunc[('funcion', 'void', ('int', 'int'))] = {'de':'s', 'df':'s'}
# Convert tuple keys to strings
dirFunc_str_keys = {str(key): value for key, value in dirFunc.items()}
# Formatted print
print(json.dumps(dirFunc_str_keys, indent=4))
