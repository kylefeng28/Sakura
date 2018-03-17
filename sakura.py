from sakuralex import *
from sakuraparse import *

# ------------------------------------------------------------------------------
# Interpreter
# ------------------------------------------------------------------------------
class SakuraInterpreter():
    def __init__(self):
        # dictionary of names
        self.names = { }
        self.functions = { 'print': print }
        self.functions = { 'input': input }

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

    def visit_UnaryOp(self, node):
        if node.value == '+':
            return +self.visit(node.operand)
        elif node.value == '-':
            return -self.visit(node.operand)

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


# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
import sys

# lexer = MyLexer(debug=1)
lexer = SakuraLexer()
parser = SakuraParser()
interpreter = SakuraInterpreter()

def parse(s):
    ast = parser.parse(s)
    print('ast: ' + str(ast)) # debug
    # print('ast: ' + repr(ast)) # debug
    if ast: interpreter.interpret(ast)

    # lexer.input(s)
    # for tok in lexer:
    #     print(tok)

def tokenize(s):
    lexer.input(s)
    for tok in lexer:
        print(tok)

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
            print(s)
        # tokenize(s)
        parse(s)

    else:
        repl()
