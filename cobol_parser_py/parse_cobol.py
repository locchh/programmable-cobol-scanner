#!/usr/bin/env python3
import sys
import os
from antlr4 import *
from Cobol85Lexer import Cobol85Lexer
from Cobol85Parser import Cobol85Parser
from Cobol85Listener import Cobol85Listener

class CobolPrintListener(Cobol85Listener):
    def __init__(self):
        self.indent = 0
        
    def enterEveryRule(self, ctx):
        rule_name = Cobol85Parser.ruleNames[ctx.getRuleIndex()]
        print('  ' * self.indent + f'Entering {rule_name}')
        self.indent += 1
        
    def exitEveryRule(self, ctx):
        self.indent -= 1
        rule_name = Cobol85Parser.ruleNames[ctx.getRuleIndex()]
        print('  ' * self.indent + f'Exiting {rule_name}')

def parse_file(file_path):
    print(f"\nParsing file: {file_path}")
    print("=" * 80)
    
    # Create an input stream from the file
    input_stream = FileStream(file_path, encoding='utf-8')
    
    # Create a lexer that feeds off of input stream
    lexer = Cobol85Lexer(input_stream)
    
    # Create a buffer of tokens pulled from the lexer
    token_stream = CommonTokenStream(lexer)
    
    # Create a parser that feeds off the tokens buffer
    parser = Cobol85Parser(token_stream)
    
    # Set error handling strategy
    #parser._errHandler = BailErrorStrategy()
    
    # Begin parsing at the startRule rule
    tree = parser.startRule()
    
    # Create a listener for walking the parse tree
    #listener = CobolPrintListener()
    
    # Walk the tree with our listener
    #walker = ParseTreeWalker()
    #walker.walk(listener, tree)
    
    
def main():
    if len(sys.argv) > 1:
        # Parse the specified file
        file_path = sys.argv[1]
        parse_file(file_path)
    else:
        print("Please provide a file path as an argument.")
        sys.exit(1)

if __name__ == '__main__':
    main()
