import ast
import re
import eplAst
import ply.yacc as yacc
import ply.lex as lex

reserved = { # reserved keywords
    'print' : 'PRINT',
    'cpt' : 'CPT', # create a channel
    'flip' : 'FLIP', # flip function
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

tokens = [ # any input get split into tokens
   'EQUALS',
   'NAME', 'STRING', 'PROBABILITY_VALUE','BIT', # bit is 0 or 1
 #  'NUMBER','INTEGER',
   'VALIDITY', 'CONDITIONING', # use PIPE instead
   'LPAREN', 'RPAREN', # brackets ( )
   'L_S_PAREN','R_S_PAREN', # square brackets []
   'L_B_PAREN','R_B_PAREN', # brace brackets {}
   'L_A_PAREN','R_A_PAREN', # brackes << >>
   'SEMICOL', 'COLON', 'COMMA',
   'PLUS', 'STAR', 'PIPE', 'KET',  # ket = >
   'CROSS','AND','NEGATE','MARGINAL' # cross = @, negate = ~, marginal = %
] +  list(reserved.values())

# Tokens
t_EQUALS = r'\='
# convention of ply: you need to use the notation t_[token name] = value.
# every token needs to be recognised through regular expression. r'\+' means take + literally.
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

# for more complex lexical recognition, you make a function
def t_NAME(t):
    # t is a token object, defined in ply. It has two properties: type and value.
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    # in Python, the first line(s) in a function definition can be (optional) a comment, of type string.
    #ply interprets these comments as instructions.
    #In this case (lexical analysis), ply reads the comment as the regular expression for that token.
    # if the input token t matches the reg exp, goes on with the function execution.
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    # If the token is a reserved word, it changes its type.
    # The above use of the method get on a dict reads as: if reserved.has_key(t.value): t.type = t.value else t.type = 'NAME'
    return t

def t_STRING(t):
    r'\"[^\"]+\"' # ^\" means any character but "
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
    pass # ignore comments, just advance in the lexical analysis (nb: with return you would stop the lexical analysis)

def t_NUMBER(t):
    r'((\d+)?\.(\d+))|\d+' # I can write .5 for 0.5
    if re.match(r'^\d+$',t.value):
        t.type = 'INTEGER'
        t.value = int(t.value)
        if t.value in [0,1]:
            t.type = 'BIT'
    else:
        t.value = float(t.value)
        if t.value <= 1.0:
            t.type = 'PROBABILITY_VALUE'
    # note that token gets typed only if the value is (a) it is integer (in which case can be INTEGER or BIT) or (b) it is <=1 (in which case it is PROBABILITY_VALUE)
    return t

t_ignore = u" \t" # ignore tabs.

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Parsing rules (definining the grammar as a BNF)

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
    # p stands for production rule (array of syntactic elements, like program, channel, SEMICOL, ...)
   'program : stm '
    # each def defines a function p_[NAME OF NON-TERMINAL SYMBOL]_[SOMETHING THAT IDENTIFIES THE CURRENT RULE]
    # As before, the first line (a Python comment) tells the matching rule, here expressed in BNF.
    # For instance, this rule says: a program is a single statement.
   p[0] = [p[1]]
    # p[0] =the property value of the syntactic element on the LHS of the rule (program, in this case)
    # p[1], p[2], ... = the property values of the syntactic elements on the RHS of the rule, in sequence
    # this says: the value of program is the array whose only element is the value of channel.

# This rule is an OR wrt the previous rule
def p_program_seqC(p):
   'program : stm SEMICOL program'
    # We use the convention of writing terminal symbols of the grammar in capital letters,
    # corresponding to the tokens of the lexical analysis.
   p[0] = p[3]
   p[0].insert(0,p[1])
    # Here I expand the array with more 'program' elements (in front). The meaning of the program will be a sequence of statements.

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

# distinction between loading (varLoad) and storing (varStore) a variable only important for compilation: at this stage they are just two names:
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
    ''' # why not something like predicate CROSS predicate? That would be is too restrictive (also because variables are not typed).
    # If we want to say 'anything that will
    # generate a predicate' this can mean many different things, so exp CROSS exp it's the sensible thing to do.
    # This is going to be too lose. At the syntactic level there is not going to be nothing
    # telling me what type the expressions are: things are made even more ambiguous because of operator overloading
    # (the same sign being used for operations on predicates and on states.) It will be the efProb execution to check
    # the types and give an error for uncorrect program.
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
        # lexer: from sequence of characters to tokens
        # lex() goes through the current program and looks for the t_SOMETHING definitions, and apply them.
        # the outcome of the lexical analysis (starting from a sequence of characters) is a sequence of tokens,
        # each with a type and a value. Now the syntactic analysis can start. It is more complex than the
        # lexical analysis (regular expression vs context free grammar)
        self.parser = yacc.yacc(start="program")
        # parser: checks if the sequence of tokens match the given syntactic rules
        # program is the name of the non-terminal symbol from which the syntactic analysis starts
        # yacc.yacc() generates the actual parser, which goes through the previous part of the program and looks for functions p_SOMETHING.
        # and builds the sequence of EfProb commands.
    def parse(self, code):
        self.lexer.input(code) # let code be the input of the lexer
        parsedListStatements = self.parser.parse(lexer=self.lexer) # this list is p[0], constructed with the production rules
        if self.single:
            syntaxTree = ast.Interactive(body=parsedListStatements)
        else:
            syntaxTree = ast.Module(body=parsedListStatements)
        ast.fix_missing_locations(syntaxTree) # Adds line numbers to the generated program. If the program gets stuck, this tells where the error was.
        return syntaxTree
