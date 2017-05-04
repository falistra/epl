import efprob_dc as efp
import lib

reserved = {
   'if' : 'IF',
   'else' : 'ELSE'
}


tokens = [
    'EQUALS',
    'NAME', 'NUMBER',
    'LPAREN', 'RPAREN',
    'L_S_PAREN','R_S_PAREN'
] +  list(reserved.values())

# Tokens
t_EQUALS = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_L_S_PAREN = r'\['
t_R_S_PAREN = r'\]'

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    return t

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

def p_statement_expr(p):
    'statement : expression'
    print(p[1])

def p_expression_if_else(p):
    'expression : IF condition then_NUMBER ELSE else_NUMBER'
    disease_dom = p[2][0] #['D', '~D']
    mood_dom = p[2][1] #['M', '~M']
    print(efp.point_pred('~D', disease_dom) @ efp.truth(mood_dom))
    p[0] = lib.IFTHENELSE(efp.point_pred('~D', disease_dom) @ efp.truth(mood_dom),
                lib.IDN(disease_dom) @ lib.IDN(mood_dom) @ lib.NEW(efp.flip(p[3])),
                lib.IDN(disease_dom) @ lib.IDN(mood_dom) @ lib.NEW(efp.flip(p[5]))) \
        * \
        lib.OBSERVE(efp.truth(disease_dom) @ efp.truth(mood_dom) @ efp.yes_pred) \
        * \
        (lib.DISCARD(disease_dom) @ lib.IDN(mood_dom) @ lib.DISCARD(efp.bool_dom))

    # solo per controllo
    print(p[0])
    w = efp.State([0.05, 0.5, 0.4, 0.05], [disease_dom, mood_dom])
    print( p[0].enforce(w) )

def p_statement_assign(p):
    'statement : NAME EQUALS expression'
    variables[p[1]] = p[3]


def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]

def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]


def p_expression_name(p):
    'expression : NAME'
    try:
        p[0] = variables[p[1]]
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0

def p_condition_number(p):
    'condition : L_S_PAREN R_S_PAREN'
    p[0] = [['D', '~D'],['M', '~M']]

def p_then_number(p):
    'then_NUMBER : NUMBER'
    p[0] = p[1]

def p_else_number(p):
    'else_NUMBER : NUMBER'
    p[0] = p[1]

def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")


import ply.yacc as yacc
yacc.yacc()

while 1:
    try:
        s = input('epl > ')
    except EOFError:
        break
    if not s:
        continue
    yacc.parse(s)
