#!/usr/bin/env python3
import os
import sys
import json
from antlr4 import *
from Cobol85CustomVisitor import Cobol85CustomVisitor
from Cobol85Lexer import Cobol85Lexer
from Cobol85Parser import Cobol85Parser


def process_cobol_file(file_path, output_dir=None):
    """
    Process a COBOL file and extract information to JSON.
    
    Args:
        file_path (str): Path to the COBOL file
        output_dir (str, optional): Directory to save the JSON output. If None, 
                                   the JSON will be saved in the same directory as the COBOL file.
    
    Returns:
        dict: The extracted program information
    """
    try:
        # Create an input stream from the file
        input_stream = FileStream(file_path, encoding='utf-8')
        
        # Create a lexer that feeds off of input stream
        from Cobol85Lexer import Cobol85Lexer
        lexer = Cobol85Lexer(input_stream)
        
        # Create a buffer of tokens pulled from the lexer
        token_stream = CommonTokenStream(lexer)
        
        # Create a parser that feeds off the tokens buffer
        parser = Cobol85Parser(token_stream)
        
        # Begin parsing at the startRule rule
        tree = parser.startRule()
        
        # Create a visitor for extracting information
        visitor = Cobol85CustomVisitor()
        program_info = visitor.visit(tree)
        
        # Determine output path
        if output_dir is None:
            output_dir = os.path.dirname(file_path)
        
        base_name = os.path.basename(file_path)
        file_name = os.path.splitext(base_name)[0]
        output_path = os.path.join(output_dir, f"{file_name}.json")
        
        # Write the JSON output
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(program_info, f, indent=2)
        
        print(f"Successfully processed {file_path}")
        print(f"JSON output saved to {output_path}")
        
        return program_info
    
    except Exception as e:
        print(f"Error processing {file_path}: {str(e)}")
        return None


def main():
    
    if len(sys.argv) < 2:
        print("Usage: python extract_source_code.py <cobol_file_or_directory> [output_directory]")
        sys.exit(1)
    
    path = sys.argv[1]
    output_dir = sys.argv[2] if len(sys.argv) > 2 else None
    
    if os.path.isfile(path):
        process_cobol_file(path, output_dir)
    else:
        print(f"Error: {path} is not a valid file or directory")
        sys.exit(1)


if __name__ == "__main__":
    main()