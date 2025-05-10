#!/usr/bin/env python3
import sys
import os
from antlr4 import *
from antlr4.error.ErrorListener import ErrorListener
from Cobol85Lexer import Cobol85Lexer
from Cobol85Parser import Cobol85Parser
from Cobol85Listener import Cobol85Listener

class CobolErrorListener(ErrorListener):
    def __init__(self):
        super(CobolErrorListener, self).__init__()
        self.errors = []

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.errors.append(f"Line {line}:{column} - {msg}")

    def reportAmbiguity(self, recognizer, dfa, startIndex, stopIndex, exact, ambigAlts, configs):
        pass

    def reportAttemptingFullContext(self, recognizer, dfa, startIndex, stopIndex, conflictingAlts, configs):
        pass

    def reportContextSensitivity(self, recognizer, dfa, startIndex, stopIndex, prediction, configs):
        pass

class CobolStructureListener(Cobol85Listener):
    def __init__(self):
        self.structure = []
        self.indent = 0
        
    def enterIdentificationDivision(self, ctx):
        self.structure.append("IDENTIFICATION DIVISION")
        
    def enterEnvironmentDivision(self, ctx):
        self.structure.append("ENVIRONMENT DIVISION")
        
    def enterDataDivision(self, ctx):
        self.structure.append("DATA DIVISION")
        
    def enterProcedureDivision(self, ctx):
        self.structure.append("PROCEDURE DIVISION")
        
    def enterProgramIdParagraph(self, ctx):
        if ctx.programName():
            program_name = ctx.programName().getText()
            self.structure.append(f"  PROGRAM-ID: {program_name}")

def parse_file(file_path, verbose=False):
    print(f"\nParsing file: {file_path}")
    print("=" * 80)
    
    # Read the file content for display
    with open(file_path, 'r', encoding='utf-8') as f:
        file_content = f.read()
    
    # Show first few lines of the file
    print("File content preview:")
    lines = file_content.split('\n')
    for i, line in enumerate(lines[:5]):
        print(f"{i+1}: {line}")
    if len(lines) > 5:
        print("...")
    
    # Create an input stream from the file
    input_stream = FileStream(file_path, encoding='utf-8')
    
    # Create a lexer that feeds off of input stream
    lexer = Cobol85Lexer(input_stream)
    
    # Create a buffer of tokens pulled from the lexer
    token_stream = CommonTokenStream(lexer)
    
    # Create a parser that feeds off the tokens buffer
    parser = Cobol85Parser(token_stream)
    
    # Add error listener
    error_listener = CobolErrorListener()
    parser.removeErrorListeners()
    parser.addErrorListener(error_listener)
    
    try:
        # Begin parsing at the startRule rule
        tree = parser.startRule()
        
        # Create a listener for extracting structure
        structure_listener = CobolStructureListener()
        
        # Walk the tree with our listener
        walker = ParseTreeWalker()
        walker.walk(structure_listener, tree)
        
        # Print the structure
        print("\nProgram Structure:")
        for item in structure_listener.structure:
            print(item)
        
        if verbose:
            # Print token information
            print("\nToken Information:")
            token_stream.fill()
            for i, token in enumerate(token_stream.tokens):
                if i < 20:  # Limit to first 20 tokens to avoid overwhelming output
                    print(f"Token {i}: {token.text} (Type: {lexer.symbolicNames[token.type] if token.type >= 0 else 'EOF'})")
            
            if len(token_stream.tokens) > 20:
                print(f"... and {len(token_stream.tokens) - 20} more tokens")
        
        print("\nParsing completed successfully!")
        return True
    except Exception as e:
        print(f"\nError parsing file: {str(e)}")
        if error_listener.errors:
            print("\nDetailed errors:")
            for error in error_listener.errors:
                print(f"  - {error}")
        
        # Try to identify the problematic line
        if hasattr(e, 'line') and hasattr(e, 'column'):
            line_num = e.line
            col_num = e.column
            if 0 <= line_num < len(lines):
                print(f"\nProblematic line ({line_num}):")
                print(lines[line_num])
                print(" " * col_num + "^")
        return False

def main():
    verbose = '--verbose' in sys.argv
    if '--verbose' in sys.argv:
        sys.argv.remove('--verbose')
    
    if len(sys.argv) > 1:
        # Parse the specified file
        file_path = sys.argv[1]
        parse_file(file_path, verbose)
    else:
        print("Please provide a file path as an argument.")
        sys.exit(1)
        

if __name__ == '__main__':
    main()
