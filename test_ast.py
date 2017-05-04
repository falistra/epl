import efprob_dc as efp
import program as libKenta
import ast

while 1:
    try:
        s = input('ep+Kenta > ')
    except EOFError:
        break
    if not s: # s is an empty (blank) line
        continue
    # s = input string (e.g. "print('Hello!')" )

    parsed = ast.parse(s)
    print(ast.dump(parsed))

    compiled = compile(parsed,filename="<ast>", mode="exec")

    exec(compiled)
