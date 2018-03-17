from sakuralex import *
from sakuraparse import *

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
