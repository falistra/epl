import ast
import re
import eplAst
import ply.yacc as yacc
import ply.lex as lex

reserved = {
    'print' : 'PRINT',
    'cpt' : 'CPT',
    'flip' : 'FLIP',
    'randomState' : 'RANDOM_STATE',
    'uniformState' : 'UNIFORM_STATE',
    'copy' : 'COPY',
    'idn'  : 'IDN',
    'swap' : 'SWAP',
    'truth' : 'TRUTH',
    'falsity' : "FALSITY",
    'True' : "TRUE",
    'False' : "FALSE"
#   'if' : 'IF',
#   'else' : 'ELSE'
}

tokens = [
   'EQUALS',
   'NAME', 'STRING', 'PROBABILITY_VALUE','BIT',
 #  'NUMBER','INTEGER',
   'VALIDITY', 'CONDITIONING', # use PIPE instead
   'LPAREN', 'RPAREN',
   'L_S_PAREN','R_S_PAREN',
   'L_B_PAREN','R_B_PAREN',
   'L_A_PAREN','R_A_PAREN',
   'SEMICOL', 'COLON', 'COMMA',
   'PLUS', 'STAR', 'PIPE', 'KET',
   'CROSS','AND','NEGATE','MARGINAL'
] +  list(reserved.values())

# Tokens
t_EQUALS = r'\='
t_SEMICOL = r'\;'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_L_S_PAREN = r'\['
t_R_S_PAREN = r'\]'
t_L_B_PAREN = r'\{'
t_R_B_PAREN = r'\}'
t_PIPE = r'\|'
t_KET = r'\>'
t_PLUS = r'\+'
t_COLON = r'\:'
t_COMMA = r'\,'
t_STAR = r'\*'
t_CROSS = r'\@'
t_AND = r'\&'
t_NEGATE = r'\~'
t_MARGINAL = r'\%'
t_CONDITIONING = r'\/' # use PIPE instead

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    return t

def t_STRING(t):
    r'\"[^\"]+\"'
    t.type = 'STRING'    # Check for reserved words
    return t

def t_VALIDITY(t):
    r'\|\='
    t.type = 'VALIDITY'
    return t

def t_L_A_PAREN(t):
    r'\<\<'
    t.type = 'L_A_PAREN'
    return t

def t_R_A_PAREN(t):
    r'\>\>'
    t.type = 'R_A_PAREN'
    return t

def t_comment(t):
    r"[ ]*\043[^\n]*"  # \043 is '#'
    pass

def t_NUMBER(t):
    r'((\d+)?\.(\d+))|\d+'
    if re.match(r'^\d+$',t.value):
        t.type = 'INTEGER'
        t.value = int(t.value)
        if t.value in [0,1]:
            t.type = 'BIT'
    else:
        t.value = float(t.value)
        if t.value <= 1.0:
            t.type = 'PROBABILITY_VALUE'
    return t

t_ignore = u" \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Parsing rules

precedence = (
    ('left', 'L_A_PAREN'),
    ('left', 'R_A_PAREN'),
    ('left', 'CROSS'),
    ('left', 'STAR'),
    ('left', 'CONDITIONING'),
    ('left', 'PIPE'),
    ('left', 'EQUALS'),
    ('left', 'AND'),
    ('left', 'PLUS'),
    ('right', 'UNEGATE')
)

def p_program_singleC(p):
   'program : stm '
   p[0] = [p[1]]

def p_program_seqC(p):
   'program : stm SEMICOL program'
   p[0] = p[3]
   p[0].insert(0,p[1])

def p_stm_print(p):
    '''stm : PRINT LPAREN exp RPAREN '''
    p[0] = eplAst.print_(p)

def p_stm_exp(p):
    '''stm : exp '''
    p[0] = eplAst.exp(p)

def p_stm_assign(p):
   '''stm : varStore EQUALS exp'''
   p[0] = eplAst.assign(p)

def p_exp_par(p):
   'exp : LPAREN exp RPAREN'
   p[0] = p[2]

def p_exp_state(p):
   '''exp : varLoad
            | validity
            | conditioning
            | state
            | stateExp
            | predicate
            | predicateExp
            | channel
            | channelExp
            | stateTransformation
            | predicateTransformation
            | builtin_function
           '''
   p[0] = p[1]

def p_varLoad(p):
    """ varLoad : NAME """
    p[0] = eplAst.varLoad(p)

def p_varStore(p):
    """ varStore : NAME """
    p[0] = eplAst.varStore(p)

def p_validity(p):
   '''validity : exp VALIDITY exp'''
   p[0] = eplAst.validity(p)

def p_conditioning(p):
   '''conditioning : exp CONDITIONING exp'''
   p[0] = eplAst.conditioning(p)

def p_stateTransformation(p):
   'stateTransformation : exp R_A_PAREN exp'
   p[0] = eplAst.stateTransformation(p)

def p_predicate_transformation(p):
   'predicateTransformation : exp L_A_PAREN exp'
   p[0] = eplAst.predicateTransformation(p)

def p_state_par(p):
   'state : L_S_PAREN stateBody R_S_PAREN'
   p[0] = eplAst.state(p)

def p_stateBody(p):
   'stateBody : stateElem PLUS stateBody'
   p[0] = p[3]
   p[0].insert(0,p[1])

def p_stateBody_singleElem(p):
   'stateBody : stateElem'
   p[0] = [p[1]]

def p_state_elem(p):
   'stateElem : PROBABILITY_VALUE PIPE nameOrBool optionKET'
   p[0] = eplAst.stateElem(p)

def p_nameOrBool(p):
    '''nameOrBool : name
                    | string
                    | boolean'''
    p[0] = p[1]

def p_name(p):
    'name : NAME'
    p[0] = eplAst.name(p)

def p_string(p):
    'string : STRING'
    p[0] = eplAst.string(p)

def p_boolean(p):
    '''boolean : TRUE
                | FALSE'''
    p[0] = eplAst.bool(p)

def p_optionKET(p):
   '''optionKET : KET
                | empty'''

def p_state_Exp(p):
    '''stateExp : exp MARGINAL dimensionSel
    '''
    p[0] = eplAst.stateExp(p)

def p_dimensionSel(p):
    """ dimensionSel : L_S_PAREN dimensionSelBody R_S_PAREN"""
    p[0] = p[2]

def p_dimensionSelBody(p):
   'dimensionSelBody : dimensionSelElem COMMA dimensionSelBody'
   p[0] = p[3]
   p[0].insert(0,p[1])

def p_dimensionSelBody_singleElem(p):
   'dimensionSelBody : dimensionSelElem'
   p[0] = [p[1]]

def p_dimensionSelElem(p):
   'dimensionSelElem : BIT'
   p[0] = eplAst.integer_value(p)

def p_predicate_par(p):
   'predicate : L_B_PAREN predicateBody R_B_PAREN'
   p[0] = eplAst.predicate(p)

def p_predicateBody(p):
   'predicateBody : predicateElem COMMA predicateBody'
   p[0] = p[3]
   p[0].insert(0,p[1])

def p_predicateBody_singleElem(p):
   'predicateBody : predicateElem'
   p[0] = [p[1]]

def p_predicate_elem(p):
   'predicateElem : nameOrBool COLON PROBABILITY_VALUE'
   p[0] = eplAst.predicateElem(p)

def p_predicate_exp(p):
    '''predicateExp : exp CROSS exp
                    | exp AND exp
                    | exp PLUS exp
                    | exp PIPE exp
                    | PROBABILITY_VALUE STAR exp
    '''
    p[0] = eplAst.predicateExp(p)

def p_predicate_negate(p):
    'predicateExp : NEGATE exp %prec UNEGATE'
    p[0] = eplAst.negate(p)

def p_channel_def(p):
    ''' channel : L_A_PAREN matrice COMMA domcod COMMA domcod R_A_PAREN '''
    p[0] = eplAst.channel(p)

def p_matrice(p):
    ''' matrice : L_S_PAREN righe R_S_PAREN '''
    p[0] = eplAst.matrice(p)

def p_righe(p):
    ''' righe  : riga COMMA righe'''
    p[0] = p[3]
    p[0].insert(0,eplAst.riga(p))

def p_righe_singleRow(p):
    ''' righe  : riga '''
    p[0] = [eplAst.riga(p)]

def p_riga(p):
    ''' riga : L_S_PAREN riga_values R_S_PAREN '''
    p[0] = p[2]

def p_riga_values_single(p):
    ''' riga_values  : PROBABILITY_VALUE '''
    p[0] = [eplAst.probability_value(p)]

def p_riga_values(p):
    ''' riga_values  : PROBABILITY_VALUE COMMA riga_values'''
    p[0] = p[3]
    p[0].insert(0,eplAst.probability_value(p))

def p_channel_Exp(p):
    '''channelExp : exp STAR exp
    '''
    p[0] = eplAst.channelExp(p)

def p_domcod(p):
    ''' domcod : L_S_PAREN domcod_values R_S_PAREN '''
    p[0] = eplAst.domcod(p)

def p_domcod_values_last(p):
    ''' domcod_values : nameOrBool '''
    p[0] = [p[1]]

def p_domcod_values(p):
    ''' domcod_values :  nameOrBool COMMA domcod_values'''
    p[0] = p[3]
    p[0].insert(0,p[1])

def p_builtin_function(p):
    '''builtin_function : FLIP LPAREN PROBABILITY_VALUE RPAREN
                    | UNIFORM_STATE LPAREN domcod_values RPAREN
                    | RANDOM_STATE LPAREN domcod_values RPAREN
                    | FALSITY LPAREN domcod_values RPAREN
                    | COPY LPAREN domcod_values RPAREN
                    | IDN LPAREN domcod_values RPAREN
                    | SWAP LPAREN domcod COMMA domcod RPAREN
                    | CPT LPAREN riga_values RPAREN
                    | TRUTH LPAREN domcod_values RPAREN'''
    p[0] = eplAst.c_builtin_function(p)

def p_empty(p):
    '''empty : '''

def p_error(p):
   if p:
      print("Syntax error at '%s'" % p.value)
   else:
      print("Syntax error at EOF")

# ====== Syntax Parsing ==================
class eplParser(object):
    def __init__(self, single=False):
        self.single = single
        self.lexer = lex.lex()
        self.parser = yacc.yacc(start="program")

    def parse(self, code):
        self.lexer.input(code)
        parsedListStatements = self.parser.parse(lexer=self.lexer)
        if self.single:
            syntaxTree = ast.Interactive(body=parsedListStatements)
        else:
            syntaxTree = ast.Module(body=parsedListStatements)
        ast.fix_missing_locations(syntaxTree)
        return syntaxTree
