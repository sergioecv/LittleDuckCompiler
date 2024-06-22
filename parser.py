import ply.yacc as yacc
from lex import Lexer
from memory import Memory
from collections import deque

class Parser:
    tokens = Lexer.tokens

    def __init__(self):
        self.dirFunc = {}
        self.varTable = {}

        self.varStack = deque()

        self.quadruples = []
        self.operatorStack = deque()
        self.operandStack = deque()
        self.typeStack = deque()
        self.jumpStack = deque()

        self.lexer = Lexer()
        self.parser = yacc.yacc(module=self)

        self.globalMemory = Memory()
        self.tempMemory = Memory()
        self.cteMemory = Memory()

        self.operatorToCode = {
            '>': 1,
            '<': 2,
            '!=': 3,
            '+': 4, 
            '-': 5, 
            '*': 6, 
            '/': 7,
            '=': 8,
            'print': 9,
            'goTo': 10,
            'goToF': 11,
            'goToT': 12,
        }

    def result_type(self, operand1, operand2, operator):
        semantic_cube = {
            ('int', 'int', "+"): 'int',
            ('int', 'int', "-"): 'int',
            ('int', 'int', "*"): 'int',
            ('int', 'int', "/"): 'float',
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
    def p_programa(self, p):
        # Body
        'programa : PROGRAM ID SEMICOLON dec_vars gl_vars dec_funcs MAIN body END'
        p[0] = ('program', p[2], p[4], p[5], ('main', p[8]))

    def p_gl_vars(self, p):
        'gl_vars : '
        # Declare global variables
        name_global = ('global', 'void')
        self.dirFunc[name_global] = self.varTable.copy()
        self.varTable.clear()
        

    def p_dec_vars(self, p):
        '''dec_vars : vars
                    | empty
        '''
        p[0] = p[1]

    def p_dec_funcs(self, p):
        '''dec_funcs : funcs dec_funcs
                    | empty
        '''
        
        if len(p) == 3:
            p[0] = (p[1], p[2])
        else:
            self.varTable = self.dirFunc[('global', 'void')].copy()

    #                                         <VARS>     
    def p_vars(self, p):
        '''vars : VAR variables
        '''
        variable, tp, variables = p[2]
        p[0] = ('var', variable, tp, variables)

    def p_variables(self, p):
        '''variables : list_ids COLON type SEMICOLON variables
                    | empty
        '''
        if len(p) != 2:
            p[0] = [p[1], p[3], p[5]]
            

    def p_list_ids(self, p):
        'list_ids : ID more_ids'
        name_id = p[1]
        if p[2] == None:
            p[0] = [name_id]
        else:
            p[0] = [name_id , p[2]]
        self.varStack.append(name_id)

    def p_more_ids(self, p):
        '''
        more_ids : COMMA list_ids
                | empty
        '''
        if len(p) != 2:
            p[0] = p[2]

    # Insert variables to varTable
    def var_insert(self, var_type):

        while (len(self.varStack) != 0):
            name_id = self.varStack.pop()

            # Validate id declaration
            if name_id not in self.varTable:
                # Associate slot memory
                memory_code = self.globalMemory.associate_memory_code(var_type)
                # Global code
                memory_code += 10000
                # Insert variable
                self.varTable[name_id] = (var_type, memory_code)
            else:
                # Error
                raise ReferenceError(f"Multiple declaration: '{name_id}'") 
            self.typeStack.clear()

    #                                         <TYPE>     
    def p_type(self, p):
        '''
        type : INT
            | FLOAT
        '''
        p[0] = p[1]
        _type = p[1]
        if len(self.varStack) > 0:
            self.var_insert(_type)

    #                                         <BODY>   
    def p_body(self, p):
        '''
        body : LBRACE statement_list RBRACE
        '''
        p[0] = p[2]

    def p_statement_list(self, p):
        '''
        statement_list : statement statement_list
            | empty
        '''
        if len(p) == 3:
            if p[2] == None:
                p[0] = p[1]
            else:
                p[0] = [p[1],p[2]]
        

    #                                         <STATEMENT>   
    def p_statement(self, p):
        '''
        statement : assign
                | condition
                | cycle
                | f_call
                | print
                | while
        '''
        p[0] = p[1]

    


    #                                         <PRINT>   
    def p_print(self, p):
        '''
        print : PRINT LPAREN dec_print RPAREN SEMICOLON
        '''
        p[0] = (p[1], p[3])

    def p_dec_print(self, p):
        '''
        dec_print : string_exp print_exp more_print
                | expresion print_exp more_print 
        '''
        if p[3] == None:
            p[0] = p[1]
        else:
            p[0] = [p[1] , p[3]]

    def p_string_exp(self, p):
        '''
        string_exp : STRING 
        '''
        string_exp = p[1]
        p[0] = string_exp
        str_exp = string_exp[1:-1]
        mem_code = self.cteMemory.assign_cte('bool', str_exp)
        mem_code += 30000
        self.operandStack.append(mem_code)

    def p_print_exp(self, p):
        '''
        print_exp : 
        '''
        elem = self.operandStack.pop()
        operatorCode = self.operatorToCode['print']
        self.quadruples.append([operatorCode, None, None, elem])

    def p_more_print(self, p):
        '''
        more_print : COMMA dec_print
                | empty
        '''
        if len(p) != 2:
            p[0] = p[2] 

    # Look for variable in global or function scope
    def lookup_var(self, name_id):
        globalTable = self.dirFunc.get( ('global', 'void'))

        if type(name_id) == int:
            return 'int'
        elif type(name_id) == float:
            return 'float'

        # If variable table is global, then look for type
        if self.varTable is globalTable:
            return current_table.get(name_id)
        # look for local and global type
        else:
            findVariable = self.varTable.get(name_id, globalTable.get(name_id))
            if (findVariable is None):
                return None, None 

            return findVariable
                 

    #                                         <ASSIGN>   
    def p_assign(self, p):
        '''
        assign : ID EQUALS expresion SEMICOLON
        '''
        # Global memory
        name_id, operator = p[1], p[2]

        # Get type from expression
        expression_type = self.typeStack.pop()

        variable_type, dirMemory = self.lookup_var(name_id)

        # Check if variable is declared in local scope
        if variable_type is None:
            raise ReferenceError(f"Variable {name_id} not declared")
        # Check if variable and expression type are valid
        elif variable_type != expression_type:
            raise TypeError(f"Expected type: {variable_type} instead of {expression_type}")
        
        p[0] = ('assign', p[1], p[3])

        # Operand
        result_operand = self.operandStack.pop()
        # Get code of operator
        operatorCode = self.operatorToCode[operator]
        # Assign quadruple
        self.quadruples.append([operatorCode, result_operand, None, dirMemory])
    #                                         <CYCLE>   
    def p_while(self, p):
        '''
        while : while_cycle LPAREN expresion RPAREN while_exp body SEMICOLON
        '''
        p[0] = (p[1], p[4], p[3], [7])
        # Get end of while jump
        end = self.jumpStack.pop()
        # Get return of while
        ret = self.jumpStack.pop()

        memCode = self.operatorToCode['goTo']
        self.quadruples.append([memCode, None, None, ret])

        # Save where the program continues after the condition
        self.quadruples[end][3] = len(self.quadruples)+1
        
    def p_while_cycle(self, p):
        '''
        while_cycle : WHILE
        '''
        # push return of while
        self.jumpStack.append(len(self.quadruples)+1)

    def p_while_exp(self, p):
        '''
        while_exp : empty 
        '''
        # Validate if expression
        exp_type = self.typeStack.pop()
        if(exp_type != 'bool'):
            raise TypeError(f"Type mismatch in expression: {exp_type}")
        else:
            # Result expression
            result = self.operandStack.pop()
            # Jump to beginning of cycle
            # return_while = self.jumpStack.pop()

            memCode = self.operatorToCode['goToF']
            self.quadruples.append([memCode, result, None, None])

            # Add quadruple jump
            self.jumpStack.append(len(self.quadruples)-1)


    def p_cycle(self, p):
        '''
        cycle : do_cycle body WHILE LPAREN expresion RPAREN SEMICOLON
        '''
        p[0] = (p[1], p[2], p[3], [5])

        # Validate if expression
        exp_type = self.typeStack.pop()
        if(exp_type != 'bool'):
            raise TypeError(f"Type mismatch in expression: {exp_type}")
        else:
            # Result expression
            result = self.operandStack.pop()
            # Jump to beginning of cycle
            return_while = self.jumpStack.pop()
            # Add quadruple jump
            memCode = self.operatorToCode['goToT']
            self.quadruples.append([memCode, result, None, return_while])

    def p_cycle_start(self, p):
        'do_cycle : DO'
        self.jumpStack.append(len(self.quadruples)+1)

    #                                         <CONDITION>   
    def p_condition(self, p):
        '''
        condition : init_condition body not_condition SEMICOLON
        '''
        p[0] = (p[1], p[2], p[3])

        # Get end of condition jump
        end = self.jumpStack.pop()
        # Save where the program continues after the condition
        self.quadruples[end][3] = len(self.quadruples)+1

    def p_init_condition(self, p):
        '''
        init_condition : IF LPAREN expresion RPAREN
        '''
        p[0] = (p[1], p[3])
        # Validate if expression
        exp_type = self.typeStack.pop()
        if(exp_type != 'bool'):
            raise TypeError(f"Type mismatch in expression: {exp_type}")
        else:
            # Result expression
            result = self.operandStack.pop()
            # Add quadruple jump
            memCode = self.operatorToCode['goToF']
            self.quadruples.append([memCode, result, None, None])
            # Keep track of jump instruction
            self.jumpStack.append(len(self.quadruples)-1)

    def p_not_condition(self, p):
        '''
        not_condition : ELSE define_jump body
                | empty
        '''
        if len(p) == 4:
            p[0] = [p[1] , p[3]]

    def p_define_jump(self, p):
        '''
        define_jump : empty
        '''
        memCode = self.operatorToCode['goTo']
        self.quadruples.append([memCode, None, None, None])

        false = self.jumpStack.pop()
        self.jumpStack.append(len(self.quadruples)-1)

        self.quadruples[false][3] = len(self.quadruples)+1

    #                                         <EXPRESSION>   
    def p_expresion(self, p):
        '''
        expresion : exp compare_expresion
        '''
        op1 = p[1]
        if p[2] != None:
            comp_op, op2 = p[2]
            p[0] = (op1,comp_op, op2)
        else:
            p[0] = (op1)

    def p_compare_expresion(self, p):
        '''
        compare_expresion : op_comp exp
                    | empty 
        '''
        if len(p) == 3:
            operator, operand = p[1], p[2]
            p[0] = [operator, operand]
            
            
    #                                         <EXP>   
    def p_exp(self, p):
        '''
        exp : termino add_sub
        '''
        op1 = p[1]
        if p[2] != None:
            operator, op2 = p[2]
            p[0] = op1 , operator, op2
        else:
            p[0] = p[1]
        self.solve_pending_operation(('<','>','!='))

    def p_add_sub(self, p):
        '''
        add_sub : op_plus_min exp
            | empty
        '''
        if len(p) == 3:
            operator, operand = p[1], p[2]
            p[0] = [operator, operand]

    def p_op_comp(self, p):
        '''
        op_comp : GREATER
                | LESS
                | NOT
        '''
        operator = p[1]
        p[0] = operator
        self.operatorStack.append(operator)

    def p_op_plus_min(self, p):
        '''
        op_plus_min : PLUS
                | MINUS
        '''
        operator = p[1]
        p[0] = operator
        self.operatorStack.append(operator)

    def p_op_times_divide(self, p):
        '''
        op_times_divide : TIMES
                | DIVIDE
        '''
        operator = p[1]
        p[0] = operator
        self.operatorStack.append(operator)

    def solve_pending_operation(self, operators_opt):
        # Temporal memory
        if self.operatorStack and (self.operatorStack[-1] in operators_opt):
            # Types
            right_type, left_type = self.typeStack.pop(), self.typeStack.pop()
            # Operands
            right_operand, left_operand = self.operandStack.pop(), self.operandStack.pop()
            # Operator
            oper = self.operatorStack.pop()

            res_type = self.result_type(left_type, right_type, oper)
            if(res_type == 'error'):
                raise TypeError(f"Type mismatch in comparison: {left_type} {oper} {right_type}")
            else:
                memory_code = self.tempMemory.associate_memory_code(res_type)
                # Temp code
                memory_code += 20000
                self.operandStack.append(memory_code)
                self.typeStack.append(res_type)
                operatorCode = self.operatorToCode[oper]
                self.quadruples.append([operatorCode, left_operand, right_operand, memory_code])

    

    #                                         <TERMINO>   
    def p_termino(self, p):
        '''
        termino : factor mult_div
        '''
        op1 = p[1]
        if p[2] != None:
            operator, op2 = p[2]
            p[0] = op1 , operator, op2
        else:
            p[0] = p[1]

        self.solve_pending_operation(('+','-'))

    def p_mult_div(self, p):
        '''
        mult_div : op_times_divide termino
                | empty
        '''
        if len(p) == 3:
            operator, operand = p[1], p[2]
            p[0] = [operator, operand]

    #                                         <FACTOR>   
    def p_factor(self, p):
        '''
        factor : LPAREN seen_lparen expresion RPAREN seen_rparen
                | MINUS dec_num
                | PLUS dec_num
                | dec_num
        '''
        if len(p) == 6:
            p[0] = p[3]
        elif len(p) == 3:
            if (p[1] == '-'):
                # Unary operator
                recentDir = self.operandStack[-1]
                cte = self.cteMemory.get_value(recentDir)
                if(cte == p[2]):
                    self.cteMemory.assign_value(recentDir, -p[2])
                    p[0] = -p[2]
            else:
                p[0] = p[2]
        else:
            p[0] = p[1]
        self.solve_pending_operation(('*','/'))

    def p_seen_lparen(self, p):
        'seen_lparen :'
        self.operatorStack.append('(')

    def p_seen_rparen(self, p):
        'seen_rparen :'
        self.operatorStack.reverse()
        self.operatorStack.remove('(')
        self.operatorStack.reverse()

    def p_dec_num(self, p):
        '''
        dec_num : cte
                | ID
        '''
        name_id = p[1]
        p[0] = name_id

        if type(name_id) != int and type(name_id) != float:
            variable_type, dirMemory = self.lookup_var(name_id)

            if variable_type is None:
                raise ReferenceError(f"Variable {name_id} not declared")
            else:
                self.operandStack.append(dirMemory)
                self.typeStack.append(variable_type)

    #                                         <CTE>   
    def p_cte(self, p):
        '''
        cte : NUMBER
            | DECIMAL
        '''
        p[0] = p[1]
        cte = p[1]
        mem_code = None
        if type(cte) == int:
            # Assign int cte to memory
            mem_code = self.cteMemory.assign_cte('int', cte)
            self.typeStack.append('int')
        else:
            # Assign float cte to memory
            mem_code = self.cteMemory.assign_cte('float', cte)
            self.typeStack.append('float')
        # Cte code
        mem_code += 30000
        self.operandStack.append(mem_code)
    #                                         <FUNCS>   
    def p_funcs(self, p):
        '''
        funcs : VOID ID LPAREN dec_params RPAREN LBRACK dec_vars body RBRACK SEMICOLON
        '''
        p[0] = (p[1], p[2], p[4], p[7], p[8])

        # Assign the name and type to dirFunc
        typeFunction, name = p[1], p[2]
        # Assign the table of variables
        self.dirFunc[(name, typeFunction)] = self.varTable.copy()
        # Clear the table of variables for new instances
        self.varTable.clear()

    def p_dec_params(self, p):
        '''
        dec_params : ID COLON type more_params
                | empty
        '''
        if len(p) != 2:
            name_id, type_id = p[1], p[3]
            p[0] = [name_id, type_id, p[4]]
            self.varStack.append(name_id)
            self.var_insert(type_id)

    def p_more_params(self, p):
        '''
        more_params : COMMA dec_params
                | empty
        '''
        if len(p) != 2:
            p[0] = p[2]

    #                                         <F_CALL>   
    def p_f_call(self, p):
        '''
        f_call : ID LPAREN dec_expresion RPAREN SEMICOLON
        '''
        name_id = p[1]
        if  name_id not in self.dirFunc:
            raise ReferenceError(f"Function {name_id} not declared")
        p[0] = ('f_call', p[1], p[3])

    def p_dec_expresion(self, p):
        '''
        dec_expresion : expresion more_expresion
                    | empty
        '''
        if len(p) != 2:
            p[0] = [p[1], p[2]]

    def p_more_expresion(self, p):
        '''
        more_expresion : COMMA dec_expresion
                    | empty
        '''
        if len(p) != 2:
            p[0] = p[2]

    def p_empty(self, p):
        'empty :'
        pass

    # Error rule for errors
    def p_error(self, p):
        raise SyntaxError(p)

    def write_file(self):
        with open('obj.txt', 'w') as f:
            for key, value in self.dirFunc.items():
                print(f'{key}: ', file=f)
                for var_name, (var_type, address) in value.items():
                    print(f'    {var_name}: {var_type}, {address}', file=f)
            print('# END DIRFUNC', file=f)

            for quadruple in self.quadruples:
                print(quadruple, file=f)
            print('# END QUADRUPLES', file=f)
            
            print(self.globalMemory.getMemorySize(), file= f)
            print('# END GLOBAL MEMORY SIZE', file=f)

            print(self.tempMemory.getMemorySize(), file= f)
            print('# END TEMPORAL MEMORY SIZE', file=f)
            
            if self.cteMemory.memory_int:
                print(self.cteMemory.memory_int, file=f)
                print("# END INT CONSTANTS", file=f)
            if self.cteMemory.memory_float:
                print(self.cteMemory.memory_float, file=f)
                print("# END FLOAT CONSTANTS", file=f)
            if self.cteMemory.memory_bool:
                print(self.cteMemory.memory_bool, file=f)
                print("# END STRING CONSTANTS", file=f)
            
    # Function to parse input text
    def parse_code(self, input_text):
        parsed = self.parser.parse(input_text)
        self.write_file()