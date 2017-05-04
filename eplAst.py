from ast import *

def name(p):
    return Str(s=p[1])

def string(p):
    return Str(s=p[1][1:-1])

def bool(p):
    if p[1] == "True":
        return NameConstant(value=True)
    else:
        return NameConstant(value=False)

def probability_value(p):
    return Num(n=p[1])

def integer_value(p):
    return Num(n=p[1])

def varStore(p):
    return Name(id=p[1], ctx=Store())

def varLoad(p):
    return Name(id=p[1], ctx=Load())

def print_(p):
    return Expr(value=Call(func=Name(id='print', ctx=Load()), args=[p[3]], keywords=[]))

def exp(p):
    return Expr(value=p[1])

def assign(p):
    return Assign(targets=[p[1]], value=p[3])

def conditioning(p):
    return BinOp(left=p[1], op=Div(), right=p[3])

def validity(p):
    return Compare(left=p[1], ops=[GtE()], comparators=[p[3]])

def stateTransformation(p):
    return BinOp(left=p[1], op=RShift(), right=p[3])

def predicateTransformation(p):
    return BinOp(left=p[1], op=LShift(), right=p[3])

def stateExp(p):
    if p[2] == "%":
        return BinOp(left=p[1], op=Mod(), right=List(elts=p[3], ctx=Load()))

def state(p):
    items = [i for (i,_) in p[2]]
    values = [v for (_,v) in p[2]]
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='State', ctx=Load()), args=[List(elts=values, ctx=Load()), List(elts=items, ctx=Load())], keywords=[])

def stateElem(p):
    return (p[3],Num(n=p[1]))

def predicate(p):
    items = [i for (i,_) in p[2]]
    values = [v for (_,v) in p[2]]
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='Predicate', ctx=Load()), args=[List(elts=values, ctx=Load()), List(elts=items, ctx=Load())], keywords=[])

def predicateElem(p):
    return (p[1],Num(n=p[3]))

def predicateExp(p):
    if (p[2] == "@"):
        return BinOp(left=p[1], op=MatMult(), right=p[3])
    if (p[2] == "&"):
        return BinOp(left=p[1], op=BitAnd(), right=p[3])
    if (p[2] == "+"):
        return BinOp(left=p[1], op=Add(), right=p[3])
    if (p[2] == "|"):
        return BinOp(left=p[1], op=BitOr(), right=p[3])
    if (p[2] == "*"):
        return BinOp(left=Num(n=p[1]), op=Mult(), right=p[3])

def negate(p):
    return UnaryOp(op=Invert(), operand=p[2])

def riga(p):
    return List(elts=p[1], ctx=Load())

def matrice(p):
    return List(elts=p[2], ctx=Load())

def channelExp(p):
    return BinOp(left=p[1], op=Mult(), right=p[3])

def domcod(p):
    return List(elts=p[2], ctx=Load())

def channel(p):
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='Channel', ctx=Load()), args=[p[2], p[4], p[6]], keywords=[])

def randomState(p):
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='random_state', ctx=Load()), args=[List(elts=p[3], ctx=Load())], keywords=[])

def uniformState(p):
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='uniform_state', ctx=Load()), args=[List(elts=p[3], ctx=Load())], keywords=[])

def truth(p):
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='truth', ctx=Load()), args=[List(elts=p[3], ctx=Load())], keywords=[])

def falsity(p):
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='falsity', ctx=Load()), args=[List(elts=p[3], ctx=Load())], keywords=[])

def flip(p):
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='flip', ctx=Load()), args=[Num(n=p[3])], keywords=[])

def cpt(p):
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='cpt', ctx=Load()), args=p[3], keywords=[])

def copy(p):
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='copy', ctx=Load()), args=[List(elts=p[3], ctx=Load())], keywords=[])

def idn(p):
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='idn', ctx=Load()), args=[List(elts=p[3], ctx=Load())], keywords=[])

def swap(p):
    return Call(func=Attribute(value=Name(id='efp', ctx=Load()), attr='swap', ctx=Load()), args=[p[3],p[5]], keywords=[])

builtin_function_dispatch = {
    'flip' : flip,
    'randomState' : randomState,
    'uniformState' : uniformState,
    'truth' : truth,
    'falsity' : falsity,
    'cpt' : cpt,
    'idn' : idn,
    'copy' : copy,
    'swap' : swap
}

def c_builtin_function(p):
    f = builtin_function_dispatch.get(p[1])
    return f(p)
