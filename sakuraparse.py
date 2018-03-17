from sakuralex import tokens
import ply.yacc as yacc

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
    def __init__(self, condtype, cond, consequent, alternate=None):
        super().__init__(value=condtype, children=(cond, consequent, alternate))

    @property
    def condtype(self): return self.value
    @property
    def cond(self): return self.children[0]
    @property
    def consequent(self): return self.children[1]
    @property
    def alternate(self): return self.children[2]

class IterationStmt(AST):
    def __init__(self, ittype, cond, body):
        super().__init__(value=ittype, children=(cond, body))

    @property
    def condtype(self): return self.value
    @property
    def cond(self): return self.children[0]
    @property
    def body(self): return self.children[1]

# ------------------------------------------------------------------------------
# Parser
# ------------------------------------------------------------------------------

def SakuraParser(**kwargs):
    # Parsing rules
    precedence = (
        ('right', 'EQUALS'),
        ('left', 'DEQUALS', 'LANG', 'LANGEQ', 'RANG', 'RANGEQ'),
        ('left', 'PLUS','MINUS'),
        ('left', 'TIMES','DIVIDE'),
        ('right', 'UPLUS','UMINUS'),
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
        'assignment_statement : assignment_expression SEMI'
        p[0] = p[1]

    def p_assignment_expression(p):
        'assignment_expression : ID EQUALS expression'
        p[0] = SetOp(p[1], p[3])

    # TODO this is pretty hacky
    def p_expression_assignment(p):
        'expression : assignment_expression'
        p[0] = p[1]

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
                                 | UNLESS expression statement
                                 | UNLESS expression statement ELSE statement
        '''
        if len(p) == 4:
            p[0] = ConditionalStmt(p[1], p[2], p[3])
        else:
            p[0] = ConditionalStmt(p[1], p[2], p[3], p[5])

    def p_iteration_statement_while(p):
        'iteration_statement : WHILE expression statement'
        p[0] = IterationStmt('while', p[2], p[3])

    def p_iteration_statement_do_while(p):
        # TODO implement all of these
        'iteration_statement : DO expression WHILE statement'
        p[0] = IterationStmt('do-while', p[2], p[4])

    def p_iteration_statement_repeat_until(p):
        # TODO implement all of these
        'iteration_statement : REPEAT expression UNTIL statement'
        p[0] = IterationStmt('repeat-until', p[2], p[4])

    def p_iteration_statement_loop(p):
        'iteration_statement : LOOP expression statement'
        p[0] = IterationStmt('loop', p[2], p[3])

    def p_iteration_statement_for(p):
        'iteration_statement : FOR LPAREN expression_statement expression_statement expression RPAREN statement'
        # TODO implement all of these
        p[0] = AST('IterationStmt', 'for', (p[3], p[4], p[5], p[7]))

    def p_error(p):
        if p:
            print("Syntax error at '%s'" % p.value)
        else:
            print("Syntax error")

    parser = yacc.yacc(**kwargs)
    return parser

class SakuraInterpreter():
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
        if node.value == 'unless': cond = not cond # flip
        if cond:
            return self.visit(node.consequent)
        elif node.alternate is not None:
            return self.visit(node.alternate)
        else:
            return None

    def visit_IterationStmt(self, node):
        if node.value == 'while':
            cond = self.visit(node.cond)
            while cond:
                self.visit(node.body)
                cond = self.visit(node.cond)
        elif node.value == 'do-while':
            self.visit(node.body)
            cond = self.visit(node.cond)
            while cond:
                self.visit(node.body)
                cond = self.visit(node.cond)
        elif node.value == 'repeat-until':
            self.visit(node.body)
            cond = self.visit(node.cond)
            while not cond:
                self.visit(node.body)
                cond = self.visit(node.cond)
        elif node.value == 'loop':
            n = self.visit(node.cond)
            for i in range(n):
                self.visit(node.body)
        elif node.value == 'for':
            n_init_stmt, n_cond, n_next, n_body = node.children[:4]

            self.visit(n_init_stmt)
            cond = self.visit(n_cond)
            while cond:
                self.visit(n_body)
                self.visit(n_next)
                cond = self.visit(n_cond)

