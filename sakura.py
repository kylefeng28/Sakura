# ------------------------------------------------------------------------------
# AST
# ------------------------------------------------------------------------------

class AST:
    def __init__(self, type=None, value=None, children=None):
        self._type = type
        if children:
             self.children = children
        else:
             self.children = [ ]
        self.value = value

    def type(self):
        return self._type or type(self).__name__

    # Return s-expr
    def __str__(self):
        s = f"({self.value}"
        for child in self.children:
            s += ' ' + str(child)
        s += ')'
        return s

    def __repr__(self):
        s = f"({self.type()}"
        if self.value: s += f":{self.value}"
        for child in self.children:
            s += ' ' + repr(child)
        s += ')'
        return s

class UnaryOp(AST):
    def __init__(self, op, operand):
        super().__init__(value=op, children=[operand])

    @property
    def operand(self): return self.children[0]

class BinOp(AST):
    def __init__(self, op, left, right):
        super().__init__(value=op, children=(left, right))

    @property
    def left(self): return self.children[0]
    @property
    def right(self): return self.children[1]

class LetOp(AST):
    def __init__(self, lhs, rhs):
        super().__init__(value='let', children=(lhs, rhs))

    @property
    def lhs(self): return self.children[0]
    @property
    def rhs(self): return self.children[1]

class SetOp(AST):
    def __init__(self, lhs, rhs):
        super().__init__(value='set', children=(lhs, rhs))

    @property
    def lhs(self): return self.children[0]
    @property
    def rhs(self): return self.children[1]

class FunctionCall(AST):
    def __init__(self, fnname, fnargs=None):
        super().__init__(value=fnname, children=fnargs)

class Literal(AST):
    def __init__(self, value):
        super().__init__(type='Literal', value=value)

    def __str__(self):
        return str(self.value)

class Bool(Literal):
    pass
class Number(Literal):
    pass
class String(Literal):
    def __str__(self):
        return '"' + self.value + '"'

class Ident(AST):
    def __init__(self, value):
        super().__init__(value=value)

    def __str__(self):
        return str(self.value)

class CompoundStmt(AST):
    def __init__(self, children=None):
        super().__init__(value='compound', children=children)

    def __str__(self):
        s = '{'
        for child in self.children:
            s += ' ' + str(child)
        s += ' }'
        return s

class ConditionalStmt(AST):
    def __init__(self, cond, consequent, alternate=None):
        super().__init__(value='if', children=(cond, consequent, alternate))

    @property
    def cond(self): return self.children[0]
    @property
    def consequent(self): return self.children[1]
    @property
    def alternate(self): return self.children[2]

class IterationStmt(AST):
    def __init__(self, cond, body):
        super().__init__(value='while', children=(cond, body))

    @property
    def cond(self): return self.children[0]
    @property
    def body(self): return self.children[1]

# ------------------------------------------------------------------------------
# Lexer
# ------------------------------------------------------------------------------
import ply.lex as lex
from ply.lex import TOKEN

# Tokens
reserved = {
    'begin'  : 'BEGIN',
    'end'    : 'END',
    'if'     : 'IF',
    'else'   : 'ELSE',
    'while'  : 'WHILE',
    'repeat' : 'REPEAT',
    'for'    : 'FOR',
    'until'  : 'UNTIL',
    'let'    : 'LET'
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

def MyLexer(**kwargs):
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

    STRING_SQ = r'"(.|[^\"])*"'
    STRING_DQ = r"'(.|[^\'])*'"
    STRING = f"({STRING_SQ}|{STRING_DQ})"
    @TOKEN(STRING)
    def t_STRING(t):
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

# ------------------------------------------------------------------------------
# Parser
# ------------------------------------------------------------------------------
import ply.yacc as yacc

def MyParser(**kwargs):
    # Parsing rules
    precedence = (
        ('right', 'UPLUS','UMINUS'),
        ('left', 'PLUS','MINUS'),
        ('left', 'TIMES','DIVIDE'),
        ('left', 'DEQUALS', 'LANG', 'LANGEQ', 'RANG', 'RANGEQ'),
        ('right', 'EQUALS'),
        )

    # def p_empty(p):
    #     'empty : '
    #     p[0] = AST(type='NoOp')

    def p_statement(p):
        '''statement : expression_statement
                     | assignment_statement
                     | declaration_statement
                     | compound_statement
                     | conditional_statement
                     | iteration_statement'''
        p[0] = p[1]

    # expression_binop
    def p_expression_arithmetic(p):
        '''expression : expression PLUS expression
                      | expression MINUS expression
                      | expression TIMES expression
                      | expression DIVIDE expression'''
        p[0] = BinOp(p[2], p[1], p[3])

    def p_expression_comparison(p):
        '''expression : expression DEQUALS expression
                      | expression LANG expression
                      | expression LANGEQ expression
                      | expression RANG expression
                      | expression RANGEQ expression'''
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

    def p_expression_bool(p):
        'expression : BOOL'
        p[0] = Bool(p[1])

    def p_expression_number(p):
        'expression : NUMBER'
        p[0] = Number(p[1])

    def p_expression_string(p):
        'expression : STRING'
        p[0] = String(p[1])

    def p_expression_id(p):
        'expression : ID'
        p[0] = Ident(p[1])

    def p_argument_expression_list(p):
        '''argument_expression_list : expression
                                    | argument_expression_list COMMA expression
        '''
        if len(p) == 2:
            p[0] = [ p[1] ]
        else: # 3
            p[0] = p[1] + [ p[3] ] # prepend

    def p_expression_function_call(p):
        '''expression : ID LPAREN RPAREN
                      | ID LPAREN argument_expression_list RPAREN
        '''
        if len(p) == 4:
            p[0] = FunctionCall(p[1])
        else:
            p[0] = FunctionCall(p[1], p[3])

    def p_declaration_statement(p):
        'declaration_statement : LET ID EQUALS expression SEMI'
        p[0] = LetOp(p[2], p[4])

    def p_assignment_statement(p):
        'assignment_statement : ID EQUALS expression SEMI'
        p[0] = SetOp(p[1], p[3])

    def p_expression_statement(p):
        '''expression_statement : SEMI
                                | expression SEMI
                                '''
        if len(p) == 3:
            p[0] = p[1]

    def p_compound_statement(p):
        """compound_statement : LBRACE RBRACE
                              | LBRACE statement_list RBRACE
        """
        if len(p) == 4:
            p[0] = CompoundStmt(p[2])
        else:
            p[0] = CompoundStmt()

    def p_statement_list(p):
        '''statement_list : statement
                          | statement_list statement'''
        if len(p) == 2:
            p[0] = [ p[1] ]
        else: # 3
            p[0] = p[1] + [ p[2] ] # prepend

    # TODO unless
    def p_conditional_statement(p):
        '''conditional_statement : IF expression statement
                                 | IF expression statement ELSE statement
        '''
        if len(p) == 4:
            p[0] = ConditionalStmt(p[2], p[3])
        else:
            p[0] = ConditionalStmt(p[2], p[3], p[5])

    # TODO until, repeat, for
    def p_iteration_statement(p):
        '''iteration_statement : WHILE expression statement
        '''

        p[0] = IterationStmt(p[2], p[3])

    def p_error(p):
        if p:
            print("Syntax error at '%s'" % p.value)
        else:
            print("Syntax error")

    parser = yacc.yacc(**kwargs)
    return parser

class MyInterpreter():
    def __init__(self):
        # dictionary of names
        self.names = { }
        self.functions = { 'print': print }

    def interpret(self, ast):
        result = self.visit(ast)
        if isinstance(result, str):
            print(f'=> "{result}"')
        else:
            print(f"=> {result}")
        return result

    def visit(self, node):
        if node is not None:
            method_name = 'visit_' + node.type()
            visitor = getattr(self, method_name, self.visit_generic)
            return visitor(node)

    def visit_generic(self, node):
        print(f'Error: No visit_{node.type()} method found')

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

        elif node.value == '==':
            return self.visit(node.left) == self.visit(node.right)
        elif node.value == '<':
            return self.visit(node.left) < self.visit(node.right)
        elif node.value == '<=':
            return self.visit(node.left) <= self.visit(node.right)
        elif node.value == '>':
            return self.visit(node.left) > self.visit(node.right)
        elif node.value == '>=':
            return self.visit(node.left) >= self.visit(node.right)

    def visit_LetOp(self, node):
        lhs = node.lhs
        if lhs in self.names:
            print(f"Error: Identifier `{lhs}` has already been declared")
            return None
        else:
            self.names[lhs] = self.visit(node.rhs)
            return node.rhs

    def visit_SetOp(self, node):
        lhs = node.lhs
        if lhs not in self.names:
            print(f"Error: Identifier `{lhs}` has not been declared")
            return None
        else:
            self.names[lhs] = self.visit(node.rhs)
            return node.rhs

    def visit_Literal(self, node):
        return node.value

    def visit_Ident(self, node):
        id = node.value
        if id in self.names:
            return self.names[id]
        else:
            print(f"ReferenceError: `{id}` is not defined")

    # TODO don't use python functions
    def visit_FunctionCall(self, node):
        id = node.value
        args = [ self.visit(child) for child in node.children ]
        if id in self.functions:
            return self.functions[id](*args)
        else:
            print(f"ReferenceError: `{id}` is not defined")

    def visit_CompoundStmt(self, node):
        result = None
        for child in node.children:
            result = self.visit(child)
        return result

    def visit_ConditionalStmt(self, node):
        cond = self.visit(node.cond)
        if cond:
            return self.visit(node.consequent)
        elif node.alternate is not None:
            return self.visit(node.alternate)
        else:
            return None

    def visit_IterationStmt(self, node):
        cond = self.visit(node.cond)
        while cond:
            self.visit(node.body)
            cond = self.visit(node.cond)

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
import sys

# lexer = MyLexer(debug=1)
lexer = MyLexer()
parser = MyParser()
interpreter = MyInterpreter()

def parse(s):
    ast = parser.parse(s)
    print('ast: ' + str(ast)) # debug
    # print('ast: ' + repr(ast)) # debug
    if ast: interpreter.interpret(ast)

    # lexer.input(s)
    # for tok in lexer:
    #     print(tok)

def repl():
    while True:
        try:
            s = input('sakura> ')
            # In interactive mode, surround inside a block
            s = '{' + s + ';}'
        except EOFError:
            break
        if not s: continue
        parse(s)

if __name__ == '__main__':
    if len(sys.argv) > 1:
        # Open file and compile
        s = ''
        with open(sys.argv[1], 'r') as f:
            s = f.read()
        parse(s)

    else:
        repl()
