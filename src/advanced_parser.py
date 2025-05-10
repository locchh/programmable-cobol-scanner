#!/usr/bin/env python3
import sys
import os
from antlr4 import *
from antlr4.error.ErrorListener import ErrorListener
from antlr4.tree.Trees import Trees
from Cobol85Lexer import Cobol85Lexer
from Cobol85Parser import Cobol85Parser
from Cobol85Listener import Cobol85Listener

class CobolErrorListener(ErrorListener):
    def __init__(self):
        super(CobolErrorListener, self).__init__()
        self.errors = []

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.errors.append(f"Line {line}:{column} - {msg}")

class CobolAnalysisListener(Cobol85Listener):
    def __init__(self):
        self.divisions = {}
        self.current_division = None
        self.current_section = None
        self.variables = []
        self.procedures = []
        
    def enterIdentificationDivision(self, ctx):
        self.current_division = "IDENTIFICATION DIVISION"
        self.divisions[self.current_division] = {}
        
    def enterEnvironmentDivision(self, ctx):
        self.current_division = "ENVIRONMENT DIVISION"
        self.divisions[self.current_division] = {}
        
    def enterDataDivision(self, ctx):
        self.current_division = "DATA DIVISION"
        self.divisions[self.current_division] = {}
        
    def enterProcedureDivision(self, ctx):
        self.current_division = "PROCEDURE DIVISION"
        self.divisions[self.current_division] = {}
    
    def enterProgramIdParagraph(self, ctx):
        if ctx.programName():
            program_name = ctx.programName().getText()
            if "IDENTIFICATION DIVISION" in self.divisions:
                self.divisions["IDENTIFICATION DIVISION"]["PROGRAM-ID"] = program_name
    
    def enterWorkingStorageSection(self, ctx):
        self.current_section = "WORKING-STORAGE SECTION"
        if self.current_division:
            self.divisions[self.current_division][self.current_section] = []
    
    def enterDataDescriptionEntryFormat1(self, ctx):
        if self.current_division == "DATA DIVISION" and self.current_section == "WORKING-STORAGE SECTION":
            level = None
            name = None
            pic = None
            value = None
            
            # Get level number
            if ctx.INTEGERLITERAL():
                level = ctx.INTEGERLITERAL().getText()
            elif ctx.LEVEL_NUMBER_77():
                level = "77"
                
            # Get data name
            if ctx.dataName():
                name = ctx.dataName().getText()
            elif ctx.FILLER():
                name = "FILLER"
                
            # Get picture clause
            for child in ctx.children:
                if isinstance(child, Cobol85Parser.DataPictureClauseContext):
                    pic = child.getText().replace("PIC", "").strip()
                elif isinstance(child, Cobol85Parser.DataValueClauseContext):
                    value_parts = []
                    for val_child in child.children:
                        if val_child.getText() not in ["VALUE", "IS"]:
                            value_parts.append(val_child.getText())
                    if value_parts:
                        value = " ".join(value_parts)
            
            if level and name:
                var_info = {
                    "level": level,
                    "name": name
                }
                if pic:
                    var_info["pic"] = pic
                if value:
                    var_info["value"] = value
                
                self.variables.append(var_info)
                if self.current_division in self.divisions and self.current_section in self.divisions[self.current_division]:
                    self.divisions[self.current_division][self.current_section].append(var_info)
    
    def enterParagraph(self, ctx):
        if ctx.paragraphName():
            para_name = ctx.paragraphName().getText()
            self.procedures.append(para_name)
            if self.current_division == "PROCEDURE DIVISION":
                self.divisions[self.current_division][para_name] = []

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
        
        # Create a listener for analyzing the code
        analysis_listener = CobolAnalysisListener()
        
        # Walk the tree with our listener
        walker = ParseTreeWalker()
        walker.walk(analysis_listener, tree)
        
        # Print the program structure
        print("\nProgram Structure:")
        for division, sections in analysis_listener.divisions.items():
            print(f"{division}")
            for section, items in sections.items():
                if section == "PROGRAM-ID":
                    print(f"  PROGRAM-ID: {items}")
                else:
                    print(f"  {section}")
                    if isinstance(items, list) and items:
                        for item in items:
                            if "level" in item and "name" in item:
                                item_str = f"    {item['level']} {item['name']}"
                                if "pic" in item:
                                    item_str += f" PIC {item['pic']}"
                                if "value" in item:
                                    item_str += f" VALUE {item['value']}"
                                print(item_str)
        
        # Print variables
        if analysis_listener.variables:
            print("\nVariables:")
            for var in analysis_listener.variables:
                var_str = f"{var['level']} {var['name']}"
                if "pic" in var:
                    var_str += f" PIC {var['pic']}"
                if "value" in var:
                    var_str += f" VALUE {var['value']}"
                print(var_str)
        
        # Print procedures
        if analysis_listener.procedures:
            print("\nProcedures:")
            for proc in analysis_listener.procedures:
                print(proc)
        
        if verbose:
            # Print parse tree
            print("\nParse Tree (simplified):")
            print(Trees.toStringTree(tree, None, parser))
        
        print("\nParsing completed successfully!")
        return True
    except Exception as e:
        print(f"\nError parsing file: {str(e)}")
        if error_listener.errors:
            print("\nDetailed errors:")
            for error in error_listener.errors:
                print(f"  - {error}")
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
        # Parse all .cbl files in the examples directory
        examples_dir = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'examples')
        success_count = 0
        total_count = 0
        
        for filename in os.listdir(examples_dir):
            if filename.endswith('.cbl'):
                total_count += 1
                file_path = os.path.join(examples_dir, filename)
                if parse_file(file_path, verbose):
                    success_count += 1
        
        print(f"\nSummary: Successfully parsed {success_count} out of {total_count} files.")

if __name__ == '__main__':
    main()
