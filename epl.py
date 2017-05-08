import efprob_dc as efp
from eplParser import eplParser

###### Compiler: Code generation ######
class eplCompiler(object):
    def __init__(self,single=False):
        self.single = single
        self.parser = eplParser(single=single)

    def compile(self, code):
        self.syntaxTree = self.parser.parse(code)
        if self.single:
            mode = 'single'
        else:
            mode = 'exec'
        return compile(self.syntaxTree,filename="<ast>", mode=mode)

###### Compiled Code Execution ######

class eplInterpreter(object):
    def __init__(self, single=False, debug=False):
        self.debug = debug
        self.single = single
        self.compiler = eplCompiler(single=self.single)

    def run(self, sourceCode):
        compiled = self.compiler.compile(sourceCode)
        exec(compiled,globals())
        if self.debug:
            import ast
            if self.programName:
                sys.stdout = open(self.programName + ".ast", "w")
            print(ast.dump(self.compiler.syntaxTree))

import argparse
import sys

if __name__ == "__main__":
    optionParser = argparse.ArgumentParser(description='Effectus Probability Language interpreter.')
    optionParser.add_argument('eplProgram', metavar='prg', nargs='?',
                    type=argparse.FileType('r'),
                    default=sys.stdin,
                    help='epl program path file (e.g. test/0.epl)')
    optionParser.add_argument('--debug', help='show debug', action="store_true")
    args = optionParser.parse_args()

    if args.debug:
        print("debug is on")

    if (args.eplProgram != sys.stdin):
        sourceCode = args.eplProgram.read()
        my_eplInterpreter = eplInterpreter(debug=args.debug)
        my_eplInterpreter.programName = args.eplProgram.name
        my_eplInterpreter.run(sourceCode)
    else:
        my_eplInterpreter = eplInterpreter(single=True,debug=args.debug)
        my_eplInterpreter.programName = None
        while 1:
            try:
                sourceCode = input('epl > ')
            except EOFError:
                break
            if not sourceCode:
                continue
            try:
                my_eplInterpreter.run(sourceCode)
            except Exception as error:
                errorMsg = str(error)
                if errorMsg.startswith("Syntax error"):
                    print(errorMsg)
                else:
                    print(error)
