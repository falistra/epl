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
        # exec mode = execution of an entire module as a program
        # single mode = interactive mode (line-by-line on the prompt)
        return compile(self.syntaxTree,filename="<ast>", mode=mode)
        # any object of type AST can be compiled with python method compile

###### Compiled Code Execution ######

class eplInterpreter(object):
    def __init__(self, single=False, debug=False):
        self.debug = debug
        self.single = single
        self.compiler = eplCompiler(single=self.single)

    def run(self, sourceCode):
        compiled = self.compiler.compile(sourceCode)
        exec(compiled,globals())
        # any object of type AST can be executed with python method exec
        # (with globals(), uses vars defined not only in compiled, but also in the global evnironment.
        # useful for line-by-line execution by prompt.)
        if self.debug:
            import ast
            if self.programName:
                sys.stdout = open(self.programName + ".ast", "w")
            print(ast.dump(self.compiler.syntaxTree))

import argparse # standard python module for command line interpreter
import sys

if __name__ == "__main__":
    optionParser = argparse.ArgumentParser(description='Effectus Probability Language interpreter.')
    optionParser.add_argument('eplProgram', metavar='prg', nargs='?',
                    type=argparse.FileType('r'),
                    default=sys.stdin,
                    help='epl program path file (e.g. test/0.epl)') # optional parameter that allows to type a program
    optionParser.add_argument('--debug', help='show debug', action="store_true") # option debug: can be true or false
    args = optionParser.parse_args() # args has two arguments: eplProgram and debut

    if args.debug:
        print("debug is on")

    if (args.eplProgram != sys.stdin): # if I launched the program on some epl file
        sourceCode = args.eplProgram.read()
        my_eplInterpreter = eplInterpreter(debug=args.debug) # creates an object of type eplInterpreter
        my_eplInterpreter.programName = args.eplProgram.name
        my_eplInterpreter.run(sourceCode)
    else:
        my_eplInterpreter = eplInterpreter(single=True,debug=args.debug) # single=True means read the program one line at a time
        my_eplInterpreter.programName = None
        while 1:
            try:
                sourceCode = input('epl > ')
            except EOFError: # what happens with crl+z
                break
            if not sourceCode: # what happens if blank line
                continue
            my_eplInterpreter.run(sourceCode)
