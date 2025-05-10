#!/usr/bin/env python3
import sys
import os
import random
from antlr4 import *
from antlr4.error.ErrorListener import ErrorListener
from Cobol85Lexer import Cobol85Lexer
from Cobol85Parser import Cobol85Parser
from Cobol85Listener import Cobol85Listener

class CobolGenerator:
    def __init__(self):
        self.program_name = "AUTOGEN"
        self.variables = []
        self.procedures = []
    
    def set_program_name(self, name):
        self.program_name = name
    
    def add_variable(self, level, name, pic=None, value=None):
        var = {
            "level": level,
            "name": name
        }
        if pic:
            var["pic"] = pic
        if value:
            var["value"] = value
        self.variables.append(var)
    
    def add_procedure(self, name, statements=None):
        proc = {
            "name": name,
            "statements": statements or []
        }
        self.procedures.append(proc)
    
    def generate_program(self):
        program = []
        
        # Identification Division
        program.append("       IDENTIFICATION DIVISION.")
        program.append(f"       PROGRAM-ID. {self.program_name}.")
        program.append("       AUTHOR. COBOL GENERATOR.")
        program.append("       DATE-WRITTEN. 2025-05-10.")
        program.append("")
        
        # Data Division
        if self.variables:
            program.append("       DATA DIVISION.")
            program.append("       WORKING-STORAGE SECTION.")
            for var in self.variables:
                line = f"       {var['level']} {var['name']}"
                if "pic" in var:
                    line += f" PIC {var['pic']}"
                if "value" in var:
                    if var.get("pic", "").startswith("X"):
                        line += f' VALUE "{var["value"]}"'
                    else:
                        line += f" VALUE {var['value']}"
                program.append(line + ".")
            program.append("")
        
        # Procedure Division
        if self.procedures:
            program.append("       PROCEDURE DIVISION.")
            for proc in self.procedures:
                program.append(f"       {proc['name']}.")
                for stmt in proc.get("statements", []):
                    program.append(f"           {stmt}")
            
            # Add STOP RUN to the last procedure if not present
            if not any("STOP RUN" in stmt for stmt in self.procedures[-1].get("statements", [])):
                program.append("           STOP RUN.")
            else:
                # Make sure the last line ends with a period
                last_stmt = program[-1]
                if not last_stmt.endswith("."):
                    program[-1] = last_stmt + "."
            program.append("")
        
        return "\n".join(program)

def generate_sample_program(program_name):
    generator = CobolGenerator()
    generator.set_program_name(program_name)
    
    # Add some variables
    generator.add_variable("01", "GREETING", "X(30)", "HELLO FROM GENERATED COBOL")
    generator.add_variable("01", "COUNTER", "9(3)", "0")
    generator.add_variable("01", "MAX-COUNT", "9(3)", "5")
    generator.add_variable("01", "RESULT-TABLE")
    generator.add_variable("05", "RESULT-ENTRY", "X(50)", "SPACES")
    generator.add_variable("05", "RESULT-VALUES")
    generator.add_variable("10", "RESULT-VALUE", "9(5)", "0")
    generator.add_variable("10", "RESULT-TEXT", "X(20)", "SPACES")
    
    # Add procedures
    generator.add_procedure("MAIN-PARA", [
        "DISPLAY GREETING",
        "PERFORM PROCESS-DATA UNTIL COUNTER > MAX-COUNT",
        "DISPLAY \"PROCESSING COMPLETE\"",
        "STOP RUN"
    ])
    
    generator.add_procedure("PROCESS-DATA", [
        "ADD 1 TO COUNTER",
        "COMPUTE RESULT-VALUE = COUNTER * 10",
        "MOVE \"ITERATION\" TO RESULT-TEXT",
        "DISPLAY \"PROCESSED ITEM: \" COUNTER"
    ])
    
    return generator.generate_program()

def parse_and_analyze(cobol_code, output_file=None):
    if output_file:
        with open(output_file, 'w') as f:
            f.write(cobol_code)
        print(f"Generated COBOL code saved to {output_file}")
    
    # Create an input stream from the string
    input_stream = InputStream(cobol_code)
    
    # Create a lexer that feeds off of input stream
    lexer = Cobol85Lexer(input_stream)
    
    # Create a buffer of tokens pulled from the lexer
    token_stream = CommonTokenStream(lexer)
    
    # Create a parser that feeds off the tokens buffer
    parser = Cobol85Parser(token_stream)
    
    try:
        # Begin parsing at the startRule rule
        tree = parser.startRule()
        print("\nParsing completed successfully!")
        
        # Print token information
        print("\nToken Information:")
        token_stream.fill()
        token_count = len(token_stream.tokens)
        print(f"Total tokens: {token_count}")
        
        # Print the first few tokens
        for i, token in enumerate(token_stream.tokens[:10]):
            print(f"Token {i}: {token.text} (Type: {lexer.symbolicNames[token.type] if token.type >= 0 else 'EOF'})")
        
        return True
    except Exception as e:
        print(f"\nError parsing generated code: {str(e)}")
        return False

def main():
    if len(sys.argv) > 1:
        program_name = sys.argv[1]
    else:
        program_name = "AUTOGEN"
    
    output_file = f"/home/loc/Works/untitled/examples/{program_name}.cbl"
    
    print(f"Generating COBOL program: {program_name}")
    cobol_code = generate_sample_program(program_name)
    
    print("\nGenerated COBOL Code:")
    print("=" * 80)
    print(cobol_code)
    
    parse_and_analyze(cobol_code, output_file)

if __name__ == '__main__':
    main()
