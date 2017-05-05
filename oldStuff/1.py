import efprob_dc as efp
import program as libKenta
import ast

reserved = {
   'skip' : 'SKIP',
   'abort' : 'ABORT',
   'if' : 'IF',
   'else' : 'ELSE',
   'observe' : 'OBSERVE',
   '_E_' : 'EXP',
   '_P_' : 'PRED'
}


tokens = [
    'EQUALS',
    'NAME', 'NUMBER',
    'LPAREN', 'RPAREN',
    'L_S_PAREN','R_S_PAREN',
    'SEMICOL'
] +  list(reserved.values())

# Tokens
t_EQUALS = r'='
t_SEMICOL = r';'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_L_S_PAREN = r'\{'
t_R_S_PAREN = r'\}'

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    return t

def t_comment(t):
    r"[ ]*\043[^\n]*"  # \043 is '#'
    pass

def t_NUMBER(t):
    r'[0-9]*\.[0-9]+'
    t.value = float(t.value)
    return t

t_ignore = u" \t"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
import ply.lex as lex
lex.lex()

# Parsing rules

variables = {}       # Dictionary of stored variables
workingChannel = None

def p_program_singleC(p):
    'program : channel '
    p[0] = [p[1]]

def p_program_seqC(p):
    'program : channel SEMICOL program'
    p[0] = p[3].append(p[1])

def p_channel_skip(p):
    'channel : SKIP'
    # skip(a)
    a = ['D', '~D']
    callSkip = ast.Expr(value=ast.Call(func=ast.Attribute(value=ast.Name(id='libKenta', ctx=ast.Load()), attr='skip', ctx=ast.Load()), args=[ast.Name(id='a', ctx=ast.Load())], keywords=[]))
#    programma = ast.Module(body=[callSkip])
#    ast.fix_missing_locations(programma)
#    exec(compile(programma,filename="<ast>", mode="exec"))

    p[0] = callSkip

def p_channel_abort(p):
    'channel : ABORT'
    p[0] = libKenta.skip

def p_channel_assign(p):
    'channel : NAME EQUALS EXP'
    p[0] = program.assign

def p_channel_if(p):
    'channel : IF LPAREN PRED RPAREN block ELSE block'
    p[0] = program.ifthenelse

def p_block_C(p):
    'block : L_S_PAREN program R_S_PAREN'
    p[0] = p[2]

def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")


import ply.yacc as yacc
yacc.yacc()

# ====== Syntax Parsing ==================
class eplParser(object):

    def __init__(self, lexer=None):
        self.lexer = lexer
        self.parser = yacc.yacc(start="file_input_end")

    def parse(self, code):
        self.lexer.input(code)
        parsedListStatements = self.parser.parse(lexer=self.lexer)
        syntaxTree = ast.Module(body=parsedListStatements)
        ast.fix_missing_locations(syntaxTree)
        return syntaxTree

###### Compiler: Code generation ######
class eplCompiler(object):

    def __init__(self):
        self.parser = eplParser()

    def compile(self, code, filename="<string>"):
        syntaxTree = self.parser.parse(code)
        return # remove
        ast.fix_missing_locations(syntaxTree)
        return compile(syntaxTree,filename="<ast>", mode="exec")

###### Compiled Code Execution ######
class eplInterpreter(object):
    def __init__(self):
        self.compiler = eplCompiler()

    def run(self, sourceCode):
        compiled = self.compiler(sourceCode)
        exec(compiled)

while 1:
    my_eplInterpreter = eplInterpreter()
    try:
        s = input('epl > ')
    except EOFError:
        break
    if not s:
        continue
    my_eplInterpreter.run(s)
    #yacc.parse(s)
