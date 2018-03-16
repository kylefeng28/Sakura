# -----------------------------------------------------------------------------
# calc.py
#
# A simple calculator with variables -- all in one file.
# -----------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Lexer
# ------------------------------------------------------------------------------

# Tokens
reserved = {
    'begin' : 'BEGIN',
    'end' : 'END',
    'if' : 'IF',
    'then' : 'THEN',
    'else' : 'ELSE',
    'while' : 'WHILE',
}

tokens = [
    'NUMBER', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'LPAREN', 'RPAREN',
    'EQUALS',
    'ID',
    'COMMENT'
] + list(reserved.values())

def MyLexer(**kwargs):
    t_PLUS   = r'\+'
    t_MINUS  = r'-'
    t_TIMES  = r'\*'
    t_DIVIDE = r'/'
    t_LPAREN = r'\('
    t_RPAREN = r'\)'
    t_EQUALS = r'='

    t_ignore_COMMENT= r'(//.*|\/\*(\*(?!\/)|[^*])*\*\/)'

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
    import ply.lex as lex
    lexer = lex.lex(**kwargs)
    return lexer

# ------------------------------------------------------------------------------
# Parser
# ------------------------------------------------------------------------------
def MyParser(**kwargs):
    # Parsing rules
    precedence = (
        ('left','PLUS','MINUS'),
        ('left','TIMES','DIVIDE'),
        ('right','UMINUS'),
        )

    # dictionary of names
    names = { }

    def p_statement_assign(t):
        'statement : ID EQUALS expression'
        names[t[1]] = t[3]

    def p_statement_expr(t):
        'statement : expression'
        print(t[1])

    def p_expression_binop(t):
        '''expression : expression PLUS expression
                      | expression MINUS expression
                      | expression TIMES expression
                      | expression DIVIDE expression'''
        if t[2] == '+'  : t[0] = t[1] + t[3]
        elif t[2] == '-': t[0] = t[1] - t[3]
        elif t[2] == '*': t[0] = t[1] * t[3]
        elif t[2] == '/': t[0] = t[1] / t[3]

    def p_expression_uminus(t):
        'expression : MINUS expression %prec UMINUS'
        t[0] = -t[2]

    def p_expression_group(t):
        'expression : LPAREN expression RPAREN'
        t[0] = t[2]

    def p_expression_number(t):
        'expression : NUMBER'
        t[0] = t[1]

    def p_expression_id(t):
        'expression : ID'
        try:
            t[0] = names[t[1]]
        except LookupError:
            print("Undefined name '%s'" % t[1])
            t[0] = 0

    def p_error(t):
        print("Syntax error at '%s'" % t.value)

    import ply.yacc as yacc
    parser = yacc.yacc(**kwargs)
    return parser

# lexer = MyLexer(debug=1)
lexer = MyLexer()
parser = MyParser()

def repl():
    while True:
        try:
            s = input('sakura > ')
        except EOFError:
            break
        if not s: continue
        parser.parse(s)

        # lexer.input(s)
        # for tok in lexer:
        #     print(tok)

if __name__ == '__main__':
    repl()
