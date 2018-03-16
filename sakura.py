# ------------------------------------------------------------------------------
# Lexer
# ------------------------------------------------------------------------------

class AST:
    def __init__(self, type, value=None, children=None):
         self.type = type
         if children:
              self.children = children
         else:
              self.children = [ ]
         self.value = value
    
    # Return s-expr
    def __str__(self):
        s = f"({self.value}"
        for child in self.children:
            s += ' ' + str(child)
        s += ')'
        return s
    
    def __repr__(self):
        s = f"({self.type}"
        if self.value: s += f":{self.value}"
        for child in self.children:
            s += ' ' + str(child)
        s += ')'
        return s

class UnaryOp(AST):
    def __init__(self, op, operand):
        super().__init__('UnaryOp',
                value=op, children=[operand])

    @property
    def operand(self): return self.children[0]

class BinOp(AST):
    def __init__(self, op, left, right):
        super().__init__('BinOp',
                value=op, children=(left, right))

    @property
    def left(self): return self.children[0]
    @property
    def right(self): return self.children[1]

class LetOp(AST):
    def __init__(self, lhs, rhs):
        super().__init__('LetOp',
                value='let', children=(lhs, rhs))

    @property
    def lhs(self): return self.children[0]
    @property
    def rhs(self): return self.children[1]

class SetOp(AST):
    def __init__(self, lhs, rhs):
        super().__init__('SetOp',
                value='set', children=(lhs, rhs))

    @property
    def lhs(self): return self.children[0]
    @property
    def rhs(self): return self.children[1]

class Number(AST):
    def __init__(self, value):
        super().__init__('Number', value=value)

    def __str__(self):
        return str(self.value)

class Ident(AST):
    def __init__(self, value):
        super().__init__('Ident', value=value)

    def __str__(self):
        return str(self.value)

# Tokens
reserved = {
    'begin' : 'BEGIN',
    'end'   : 'END',
    'if'    : 'IF',
    'then'  : 'THEN',
    'else'  : 'ELSE',
    'while' : 'WHILE',
    'let'   : 'LET',
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
        ('left', 'PLUS','MINUS'),
        ('left', 'TIMES','DIVIDE'),
        ('right', 'UPLUS','UMINUS'),
        ('right', 'EQUALS'),
        )

    def p_statement_declare(t):
        'statement : LET ID EQUALS expression'
        t[0] = LetOp(t[2], t[4])
        print(t[0])

    def p_statement_assign(t):
        'statement : ID EQUALS expression'
        t[0] = SetOp(t[1], t[3])
        print(t[0])

    def p_statement_expr(t):
        'statement : expression'
        print(t[1])
        t[0] = t[1]

    def p_expression_binop(t):
        '''expression : expression PLUS expression
                      | expression MINUS expression
                      | expression TIMES expression
                      | expression DIVIDE expression'''
        t[0] = BinOp(t[2], t[1], t[3])

    def p_expression_uplus(t):
        'expression : PLUS expression %prec UPLUS'
        t[0] = UnaryOp('+', t[2])

    def p_expression_uminus(t):
        'expression : MINUS expression %prec UMINUS'
        t[0] = UnaryOp('-', t[2])

    def p_expression_group(t):
        'expression : LPAREN expression RPAREN'
        t[0] = t[2]

    def p_expression_number(t):
        'expression : NUMBER'
        t[0] = Number(t[1])

    def p_expression_id(t):
        'expression : ID'
        t[0] = Ident(t[1])

    def p_error(t):
        print("Syntax error at '%s'" % t.value)

    import ply.yacc as yacc
    parser = yacc.yacc(**kwargs)
    return parser

class MyInterpreter():
    def __init__(self):
        # dictionary of names
        self.names = { }
    
    def interpret(self, ast):
        result = self.visit(ast)
        print(result)
        return result

    def visit(self, node):
        method_name = 'visit_' + node.type
        visitor = getattr(self, method_name, self.visit_generic)
        return visitor(node)

    def visit_generic(self, node):
        print(f'Error: No visit_{node.type} method found')
    
    def visit_BinOp(self, node):
        if node.value == '+':
            return self.visit(node.left) + self.visit(node.right)
        elif node.value == '-':
            return self.visit(node.left) - self.visit(node.right)
        elif node.value == '*':
            return self.visit(node.left) * self.visit(node.right)
        elif node.value == '/':
            return self.visit(node.left) / self.visit(node.right)

    def visit_LetOp(self, node):
        lhs = node.lhs
        if lhs in self.names:
            print(f"Error: Identifier `{lhs}` has already been declared")
            return None
        else:
            self.names[lhs] = node.rhs
            return node.rhs

    def visit_SetOp(self, node):
        lhs = node.lhs
        if lhs not in self.names:
            print(f"Error: Identifier `{lhs}` has not been declared")
            return None
        else:
            self.names[lhs] = node.rhs
            return node.rhs

    def visit_Number(self, node):
        return node.value

    def visit_Ident(self, node):
        id = node.value
        if id in self.names:
            return self.names[id]
        else:
            print(f"ReferenceError: `{id}` is not defined")

# lexer = MyLexer(debug=1)
lexer = MyLexer()
parser = MyParser()
interpreter = MyInterpreter()

def repl():
    while True:
        try:
            s = input('sakura> ')
        except EOFError:
            break
        if not s: continue
        ast = parser.parse(s)
        if ast: interpreter.interpret(ast)

        # lexer.input(s)
        # for tok in lexer:
        #     print(tok)

if __name__ == '__main__':
    repl()
