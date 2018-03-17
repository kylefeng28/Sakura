import ply.lex as lex
from ply.lex import TOKEN

# ------------------------------------------------------------------------------
# Lexer
# ------------------------------------------------------------------------------

# Tokens
reserved = {
    'begin'  : 'BEGIN',
    'end'    : 'END',
    'if'     : 'IF',
    'unless' : 'UNLESS',
    'else'   : 'ELSE',
    'while'  : 'WHILE',
    'do'     : 'DO',
    'until'  : 'UNTIL',
    'repeat' : 'REPEAT',
    'for'    : 'FOR',
    'loop'   : 'LOOP',
    'let'    : 'LET',
    'yield'  : 'yield'
}

tokens = [
    'BOOL', 'NUMBER', 'STRING',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE',
    'EQUALS', 'SEMI', 'COMMA',
    'DEQUALS', 'LANG', 'LANGEQ', 'RANG', 'RANGEQ',

    'ID',
    'COMMENT',
] + list(reserved.values())

def SakuraLexer(**kwargs):
    t_PLUS     = r'\+'
    t_MINUS    = r'-'
    t_TIMES    = r'\*'
    t_DIVIDE   = r'/'
    t_LPAREN   = r'\('
    t_RPAREN   = r'\)'
    t_LBRACE   = r'\{'
    t_RBRACE   = r'\}'
    t_EQUALS   = r'='
    t_SEMI     = r';'
    t_COMMA    = r','

    t_DEQUALS  = r'=='
    t_LANG     = r'<'
    t_LANGEQ   = r'<='
    t_RANG     = r'>'
    t_RANGEQ   = r'>='


    t_ignore_COMMENT= r'(//.*|\/\*(\*(?!\/)|[^*])*\*\/)'

    def t_BOOL(t):
        r'(true|false)'
        t.value = t.value == 'true'
        return t

    def t_ID(t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        t.type = reserved.get(t.value, 'ID')
        return t

    def t_NUMBER(t):
        r'\d+'
        try:
            t.value = int(t.value)
        except ValueError:
            print("Integer value too large %d", t.value)
            t.value = 0
        return t

    # this is pretty broken
    # STRING_SQ = r'"(.|[^\"])*"'
    # STRING_DQ = r"'(.|[^\'])*'"
    # STRING = f"({STRING_SQ}|{STRING_DQ})"
    # @TOKEN(STRING)
    # Taken from https://github.com/dabeaz/ply/blob/master/example/GardenSnake/GardenSnake.py
    def t_STRING(t):
        r"'([^\\']+|\\'|\\\\)*'"
        t.value = eval(t.value)
        return t

    # Ignored characters
    t_ignore = " \t"

    # Keep track of line numbers
    def t_newline(t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    # Error handling
    def t_error(t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    # Compute column
    #   input is the input text string
    #   token is a token instance
    def find_column(input, token):
        line_start = input.rfind('\n', 0, token.lexpos) + 1
        return (token.lexpos - line_start) + 1

    # EOF handling?

    # Build the lexer
    lexer = lex.lex(**kwargs)
    return lexer

