import ply.lex as lex
from ply.lex import TOKEN

# Lista de palabras reservadas
reserved = {
   'program' : 'PROGRAM',
   'main' : 'MAIN',
   'end' : 'END',
   'void' : 'VOID',
   'if' : 'IF',
   'else' : 'ELSE',
   'while' : 'WHILE',
   'do' : 'DO',
   'var' : 'VAR',
   'print' : 'PRINT',
   'int' : 'INT',
   'float' : 'FLOAT',
}


class Lexer(object):
    #Lista de tokens
    tokens = [
    'ID',
    'NUMBER',
    'DECIMAL',
    'STRING',

    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'EQUALS',

    'LESS',
    'GREATER',
    'NOT',

    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'LBRACK',
    'RBRACK',

    'COMMA',
    'COLON',
    'SEMICOLON',
    ] + list(reserved.values())
    #Expresiones regulares simples
    t_PLUS    = r'\+'
    t_MINUS   = r'-'
    t_TIMES   = r'\*'
    t_DIVIDE  = r'/'
    t_EQUALS   = r'='

    t_LESS    = r'<'
    t_GREATER    = r'>'
    t_NOT     = r'!='

    t_LPAREN  = r'\('
    t_RPAREN  = r'\)'
    t_LBRACE  = r'\{'
    t_RBRACE  = r'\}'
    t_LBRACK  = r'\['
    t_RBRACK  = r'\]'

    t_COMMA  = r','
    t_COLON  = r':'
    t_SEMICOLON  = r';'

    # A string containing ignored characters (spaces and tabs)
    t_ignore  = ' \t'

    # Funciones de expresiones regulares

    # ID debe comenzar con letra o underscore
    def t_ID(self, t):
        r'[a-zA-Z_]\w*'
        t.type = reserved.get(t.value,'ID')
        return t

    # DECIMAL puede tener solo digitos
    #       puede tener punto, pero debe tener digitos despues 
    # -puede tener un digito antes del punto, pero debe haber digitos deespues del punto-
    def t_DECIMAL(self, t):
        r'\d+\.\d+'
        t.value = float(t.value)
        return t

    # NUMBER debe tener digito
    def t_NUMBER(self, t):
        r'\d+'
        t.value = int(t.value)
        return t

    # STRING puede tener texto adentro de " "
    def t_STRING(self, t):
        r'"[^"]*"'
        t.value = str(t.value)
        return t

    # Define a rule so we can track line INTs
    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    # Error handling rule
    def t_error(self, t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    def __init__(self):
        # Build the lexer
        self.lexer = lex.lex(module=self)