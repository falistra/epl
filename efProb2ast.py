import efprob_dc as efp
import program as libKenta
import ast

import argparse
import sys

if __name__ == "__main__":
    optionParser = argparse.ArgumentParser(description='Effectus Probability Language interpreter.')
    optionParser.add_argument('efpProgram', metavar='prg', nargs='?',
                    type=argparse.FileType('r'),
                    default=sys.stdin,
                    help='efp program path file (e.g. test/0.efp.py)')
    args = optionParser.parse_args()

    sourceCode = args.efpProgram.read()
    sys.stdout = open(args.efpProgram.name + ".ast", "w")
    parsed = ast.parse(sourceCode)
    print(ast.dump(parsed))
    compiled = compile(parsed,filename="<ast>", mode="exec")
    exec(compiled)
