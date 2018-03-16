# ------------------------------------------------------------------------------
# AST
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

# ------------------------------------------------------------------------------
# Lexer
# ------------------------------------------------------------------------------

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
    'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE',
    'EQUALS', 'SEMI',
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
    t_LBRACE = r'\{'
    t_RBRACE = r'\}'
    t_EQUALS = r'='
    t_SEMI   = r';'

    t_ignore_COMMENT= r'(//.*|\/\*(\*(?!\/)|[^*])*\*\/)'

    def t_ID(p):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        p.type = reserved.get(p.value, 'ID')
        return t

    def t_NUMBER(p):
        r'\d+'
        try:
            p.value = int(p.value)
        except ValueError:
            print("Integer value too large %d", p.value)
            p.value = 0
        return p

    # Ignored characters
    t_ignore = " \t"

    # Keep track of line numbers
    def t_newline(p):
        r'\n+'
        p.lexer.lineno += len(p.value)

    # Error handling
    def t_error(p):
        print("Illegal character '%s'" % p.value[0])
        p.lexer.skip(1)

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

    def p_statement(p):
        '''statement : assignment_statement
                     | declaration_statement
                     | expression_statement
                     | compound_statement
                     | empty
                     | statement SEMI
        '''
        p[0] = p[1]

    def p_declaration_statement(p):
        'declaration_statement : LET ID EQUALS expression'
        p[0] = LetOp(p[2], p[4])

    def p_assignment_statement(p):
        'assignment_statement : ID EQUALS expression'
        p[0] = SetOp(p[1], p[3])

    def p_expression_statement(p):
        '''expression_statement : expression
                                '''
        # TODO make SEMI required
        p[0] = p[1]

    def p_compound_statement(p):
        """compound_statement : LBRACE RBRACE
                              | LBRACE statement_list RBRACE
        """
        if len(p) == 4:
            p[0] = AST(type='CompoundStmt', children=p[2])

    def p_statement_list(p):
        '''statement_list : statement
                          | statement_list statement'''
        # TODO?
        if len(p) == 3:
            p[0] = p[1] + [ p[2] ]
        else:
            p[0] = [ p[1] ]

    def p_expression_binop(p):
        '''expression : expression PLUS expression
                      | expression MINUS expression
                      | expression TIMES expression
                      | expression DIVIDE expression'''
        p[0] = BinOp(p[2], p[1], p[3])

    def p_expression_uplus(p):
        'expression : PLUS expression %prec UPLUS'
        p[0] = UnaryOp('+', p[2])

    def p_expression_uminus(p):
        'expression : MINUS expression %prec UMINUS'
        p[0] = UnaryOp('-', p[2])

    def p_expression_group(p):
        'expression : LPAREN expression RPAREN'
        p[0] = p[2]

    def p_expression_number(p):
        'expression : NUMBER'
        p[0] = Number(p[1])

    def p_expression_id(p):
        'expression : ID'
        p[0] = Ident(p[1])

    def p_empty(p):
        'empty : '
        p[0] = AST(type='NoOp')

    def p_error(p):
        print("Syntax error at '%s'" % p.value)

    import ply.yacc as yacc
    parser = yacc.yacc(**kwargs)
    return parser

class MyInterpreter():
    def __init__(self):
        # dictionary of names
        self.names = { }
    
    def interpret(self, ast):
        result = self.visit(ast)
        print(f"=> {result}")
        return result

    def visit(self, node):
        method_name = 'visit_' + node.type
        visitor = getattr(self, method_name, self.visit_generic)
        return visitor(node)

    def visit_generic(self, node):
        print(f'Error: No visit_{node.type} method found')

    def visit_NoOp(self, node):
        pass
    
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
            self.names[lhs] = self.visit(node.rhs)
            return node.rhs

    def visit_Number(self, node):
        return node.value

    def visit_Ident(self, node):
        id = node.value
        if id in self.names:
            return self.names[id]
        else:
            print(f"ReferenceError: `{id}` is not defined")

    def visit_CompoundStmt(self, node):
        result = None
        for child in node.children:
            result = self.visit(child)
        return result

# lexer = MyLexer(debug=1)
lexer = MyLexer()
parser = MyParser()
interpreter = MyInterpreter()

def repl():
    while True:
        try:
            s = input('sakura> ')
            # In interactive mode, surround inside a block
            s = '{' + s + '}'
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
