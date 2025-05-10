#!/usr/bin/env python3
"""
COBOL Extractor
A command-line application that extracts information from COBOL programs
according to a YAML configuration and outputs it in JSON format.
"""

import os
import sys
import re
import json
import yaml
import argparse
from typing import Dict, List, Any, Optional
from datetime import datetime

from antlr4 import *
from Cobol85Lexer import Cobol85Lexer
from Cobol85Parser import Cobol85Parser
from Cobol85Visitor import Cobol85Visitor


class CobolExtractorVisitor(Cobol85Visitor):
    """
    A visitor for COBOL programs that extracts information according to a YAML configuration.
    """
    def __init__(self, config_data):
        super().__init__()
        self.config = config_data
        self.result = {
            "metadata": {
                "extraction_date": datetime.now().isoformat(),
                "config_file": "yaml-based configuration"
            }
        }
        
        # Initialize sections based on configuration
        self._initialize_sections()
        
        # Track current context
        self.current_division = None
        self.current_section = None
        self.current_paragraph = None
        
        # For analysis
        self.variables = {}
        self.paragraph_calls = {}
        self.data_dependencies = {}
    
    def _initialize_sections(self):
        """Initialize result structure based on configuration"""
        # Always include these basic sections
        if self.config.get('extraction', {}).get('identification', {}).get('enabled', True):
            self.result['identification'] = {}
        
        if self.config.get('extraction', {}).get('environment', {}).get('enabled', True):
            self.result['environment'] = {}
        
        if self.config.get('extraction', {}).get('data', {}).get('enabled', True):
            self.result['data'] = {}
            for section in self.config.get('extraction', {}).get('data', {}).get('sections', []):
                if section.get('enabled', True):
                    self.result['data'][section['name']] = []
        
        if self.config.get('extraction', {}).get('procedure', {}).get('enabled', True):
            self.result['procedure'] = {
                'paragraphs': {},
                'sections': {}
            }
        
        if self.config.get('extraction', {}).get('custom_analysis', {}).get('enabled', True):
            self.result['custom_analysis'] = {}
            for element in self.config.get('extraction', {}).get('custom_analysis', {}).get('elements', []):
                self.result['custom_analysis'][element] = {}
    
    def visitStartRule(self, ctx):
        """Visit the start rule"""
        self.visitChildren(ctx)
        
        # Perform custom analysis if enabled
        if self.config.get('extraction', {}).get('custom_analysis', {}).get('enabled', True):
            self._perform_custom_analysis()
        
        return self.result
    
    def visitIdentificationDivision(self, ctx):
        """Visit the identification division"""
        if not self.config.get('extraction', {}).get('identification', {}).get('enabled', True):
            return None
        
        self.current_division = "identification"
        return self.visitChildren(ctx)
    
    def visitProgramIdParagraph(self, ctx):
        """Visit the program ID paragraph"""
        if not self.config.get('extraction', {}).get('identification', {}).get('enabled', True):
            return None
        
        elements = self.config.get('extraction', {}).get('identification', {}).get('elements', [])
        if "program_id" not in elements:
            return None
        
        if ctx.programName():
            program_name = ctx.programName().getText()
            self.result["identification"]["program_id"] = program_name
        
        return self.visitChildren(ctx)
    
    def visitAuthorParagraph(self, ctx):
        """Visit the author paragraph"""
        if not self.config.get('extraction', {}).get('identification', {}).get('enabled', True):
            return None
        
        elements = self.config.get('extraction', {}).get('identification', {}).get('elements', [])
        if "author" not in elements:
            return None
        
        if ctx.getChildCount() > 2:  # AUTHOR. <name>
            author = ctx.getChild(2).getText()
            self.result["identification"]["author"] = author
        
        return self.visitChildren(ctx)
    
    def visitDateWrittenParagraph(self, ctx):
        """Visit the date written paragraph"""
        if not self.config.get('extraction', {}).get('identification', {}).get('enabled', True):
            return None
        
        elements = self.config.get('extraction', {}).get('identification', {}).get('elements', [])
        if "date_written" not in elements:
            return None
        
        if ctx.getChildCount() > 2:  # DATE-WRITTEN. <date>
            date_written = ctx.getChild(2).getText()
            self.result["identification"]["date_written"] = date_written
        
        return self.visitChildren(ctx)
    
    def visitDataDivision(self, ctx):
        """Visit the data division"""
        if not self.config.get('extraction', {}).get('data', {}).get('enabled', True):
            return None
        
        self.current_division = "data"
        return self.visitChildren(ctx)
    
    def visitWorkingStorageSection(self, ctx):
        """Visit the working storage section"""
        if not self.config.get('extraction', {}).get('data', {}).get('enabled', True):
            return None
        
        sections = self.config.get('extraction', {}).get('data', {}).get('sections', [])
        section_enabled = False
        for section in sections:
            if section.get('name') == 'working_storage' and section.get('enabled', True):
                section_enabled = True
                break
        
        if not section_enabled:
            return None
        
        self.current_section = "working_storage"
        result = self.visitChildren(ctx)
        self.current_section = None
        return result
    
    def visitFileSection(self, ctx):
        """Visit the file section"""
        if not self.config.get('extraction', {}).get('data', {}).get('enabled', True):
            return None
        
        sections = self.config.get('extraction', {}).get('data', {}).get('sections', [])
        section_enabled = False
        for section in sections:
            if section.get('name') == 'file_section' and section.get('enabled', True):
                section_enabled = True
                break
        
        if not section_enabled:
            return None
        
        self.current_section = "file_section"
        result = self.visitChildren(ctx)
        self.current_section = None
        return result
    
    def visitLinkageSection(self, ctx):
        """Visit the linkage section"""
        if not self.config.get('extraction', {}).get('data', {}).get('enabled', True):
            return None
        
        sections = self.config.get('extraction', {}).get('data', {}).get('sections', [])
        section_enabled = False
        for section in sections:
            if section.get('name') == 'linkage_section' and section.get('enabled', True):
                section_enabled = True
                break
        
        if not section_enabled:
            return None
        
        self.current_section = "linkage_section"
        result = self.visitChildren(ctx)
        self.current_section = None
        return result
    
    def visitDataDescriptionEntryFormat1(self, ctx):
        """Visit a data description entry"""
        if not self.config.get('extraction', {}).get('data', {}).get('enabled', True):
            return None
        
        if not self.current_section:
            return None
        
        # Find the section configuration
        section_config = None
        for section in self.config.get('extraction', {}).get('data', {}).get('sections', []):
            if section.get('name') == self.current_section:
                section_config = section
                break
        
        if not section_config or not section_config.get('enabled', True):
            return None
        
        # Get fields to extract
        fields_to_extract = section_config.get('extract_fields', [])
        
        # Get level numbers to extract
        level_numbers = section_config.get('extract_level_numbers', [])
        
        # Create data item
        data_item = {}
        
        # Get level number
        level = None
        if ctx.INTEGERLITERAL():
            level = ctx.INTEGERLITERAL().getText()
            if level_numbers and int(level) not in level_numbers:
                return self.visitChildren(ctx)
            data_item["level"] = level
        elif ctx.LEVEL_NUMBER_77():
            level = "77"
            if level_numbers and 77 not in level_numbers:
                return self.visitChildren(ctx)
            data_item["level"] = level
        
        # Get data name
        if "name" in fields_to_extract:
            if ctx.dataName():
                name = ctx.dataName().getText()
                data_item["name"] = name
                
                # Store variable for analysis
                self.variables[name] = {
                    "level": level,
                    "section": self.current_section
                }
            elif ctx.FILLER():
                data_item["name"] = "FILLER"
        
        # Process picture clause
        if "picture" in fields_to_extract:
            for i in range(ctx.getChildCount()):
                child = ctx.getChild(i)
                if isinstance(child, Cobol85Parser.DataPictureClauseContext):
                    if child.pictureString():
                        pic = child.pictureString().getText()
                        data_item["picture"] = pic.replace("PIC", "").strip()
        
        # Process value clause
        if "value" in fields_to_extract:
            for i in range(ctx.getChildCount()):
                child = ctx.getChild(i)
                if isinstance(child, Cobol85Parser.DataValueClauseContext):
                    value_text = child.getText()
                    value_text = value_text.replace("VALUE", "").replace("IS", "").strip()
                    if value_text.startswith('"') and value_text.endswith('"'):
                        value_text = value_text[1:-1]
                    data_item["value"] = value_text
        
        # Add data item to result
        if data_item:
            self.result["data"][self.current_section].append(data_item)
        
        return self.visitChildren(ctx)
    
    def visitProcedureDivision(self, ctx):
        """Visit the procedure division"""
        if not self.config.get('extraction', {}).get('procedure', {}).get('enabled', True):
            return None
        
        self.current_division = "procedure"
        return self.visitChildren(ctx)
    
    def visitParagraph(self, ctx):
        """Visit a paragraph"""
        if not self.config.get('extraction', {}).get('procedure', {}).get('enabled', True):
            return None
        
        elements = self.config.get('extraction', {}).get('procedure', {}).get('elements', [])
        if "paragraphs" not in elements:
            return None
        
        if ctx.paragraphName():
            paragraph_name = ctx.paragraphName().getText()
            self.current_paragraph = paragraph_name
            
            # Add paragraph to result
            self.result["procedure"]["paragraphs"][paragraph_name] = {
                "statements": []
            }
            
            # Process sentences
            statement_types = self.config.get('extraction', {}).get('procedure', {}).get('statement_types', [])
            
            for i in range(ctx.getChildCount()):
                child = ctx.getChild(i)
                if isinstance(child, Cobol85Parser.SentenceContext):
                    for j in range(child.getChildCount()):
                        stmt = child.getChild(j)
                        if isinstance(stmt, Cobol85Parser.StatementContext):
                            # Get the statement text with proper formatting
                            stmt_text = self._format_statement(stmt.getText())
                            
                            # Check if this statement type should be extracted
                            if not statement_types or any(stmt_type in stmt_text.upper() for stmt_type in statement_types):
                                self.result["procedure"]["paragraphs"][paragraph_name]["statements"].append(stmt_text)
                                
                                # Track paragraph calls for analysis
                                if "PERFORM" in stmt_text.upper():
                                    match = re.search(r'PERFORM\s+([A-Za-z0-9-]+)', stmt_text.upper())
                                    if match:
                                        called_paragraph = match.group(1)
                                        if paragraph_name not in self.paragraph_calls:
                                            self.paragraph_calls[paragraph_name] = []
                                        self.paragraph_calls[paragraph_name].append(called_paragraph)
                                
                                # Track variable references for data dependencies
                                if self.config.get('extraction', {}).get('custom_analysis', {}).get('enabled', True):
                                    elements = self.config.get('extraction', {}).get('custom_analysis', {}).get('elements', [])
                                    if "data_dependencies" in elements:
                                        self._analyze_data_dependencies(stmt_text, paragraph_name)
        
        result = self.visitChildren(ctx)
        self.current_paragraph = None
        return result
    
    def _analyze_data_dependencies(self, stmt_text, paragraph_name):
        """Analyze data dependencies in a statement"""
        # Extract variable names from the statement
        var_pattern = r'\b([A-Za-z][A-Za-z0-9-]*)\b'
        variables = re.findall(var_pattern, stmt_text)
        
        # Filter out COBOL keywords
        keywords = ["ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "COMPUTE", "MOVE", "PERFORM", 
                   "DISPLAY", "ACCEPT", "CALL", "IF", "ELSE", "END-IF", "EVALUATE", "WHEN", 
                   "END-EVALUATE", "TO", "FROM", "BY", "GIVING", "USING", "VARYING", "UNTIL", 
                   "ON", "SIZE", "ERROR", "NOT", "OTHER"]
        
        variables = [var for var in variables if var.upper() not in keywords]
        
        # Identify target variable (left side of assignment)
        target_var = None
        if "MOVE" in stmt_text.upper():
            parts = stmt_text.upper().split("TO")
            if len(parts) > 1:
                target_vars = re.findall(var_pattern, parts[1])
                if target_vars:
                    target_var = target_vars[0]
        elif "COMPUTE" in stmt_text.upper():
            parts = stmt_text.upper().split("=")
            if len(parts) > 1:
                target_vars = re.findall(var_pattern, parts[0])
                if target_vars:
                    target_var = target_vars[0]
        
        # If we found a target variable, record dependencies
        if target_var:
            source_vars = [var for var in variables if var.upper() != target_var.upper()]
            if target_var not in self.data_dependencies:
                self.data_dependencies[target_var] = []
            
            for source_var in source_vars:
                if source_var not in self.data_dependencies[target_var]:
                    self.data_dependencies[target_var].append({
                        "source": source_var,
                        "paragraph": paragraph_name,
                        "statement": stmt_text
                    })
    
    def _perform_custom_analysis(self):
        """Perform custom analysis based on configuration"""
        elements = self.config.get('extraction', {}).get('custom_analysis', {}).get('elements', [])
        
        if "paragraph_calls" in elements:
            self.result["custom_analysis"]["paragraph_calls"] = self.paragraph_calls
        
        if "variable_usage" in elements:
            self.result["custom_analysis"]["variable_usage"] = self.variables
        
        if "data_dependencies" in elements:
            self.result["custom_analysis"]["data_dependencies"] = self.data_dependencies
    
    def _format_statement(self, text):
        """Format a COBOL statement for better readability"""
        if not text:
            return text
        
        # Preserve string literals
        string_literals = []
        text_without_strings = ""
        i = 0
        
        # Process text to extract string literals
        while i < len(text):
            if text[i:i+1] == '"':
                # Find the end of the string literal
                end_quote_pos = text.find('"', i+1)
                if end_quote_pos == -1:  # No closing quote found
                    text_without_strings += text[i:]
                    break
                
                # Extract the string literal including quotes
                string_literal = text[i:end_quote_pos+1]
                string_literals.append(string_literal)
                text_without_strings += f" __STR{len(string_literals)-1}__ "
                i = end_quote_pos + 1
            else:
                text_without_strings += text[i]
                i += 1
        
        # Add spaces around COBOL keywords
        keywords = [
            # Verbs
            "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "COMPUTE", "MOVE", "DISPLAY", "ACCEPT", "CALL",
            # Control flow
            "IF", "ELSE", "END-IF", "EVALUATE", "WHEN", "OTHER", "END-EVALUATE", "PERFORM", "UNTIL",
            # Prepositions
            "TO", "FROM", "BY", "GIVING", "USING", "INTO", "OF",
            # Error handling
            "ON", "SIZE", "ERROR", "NOT"
        ]
        
        # Split the text into tokens based on spaces and punctuation
        tokens = []
        current_token = ""
        for char in text_without_strings:
            if char.isalnum() or char == '-':
                current_token += char
            else:
                if current_token:
                    tokens.append(current_token)
                    current_token = ""
                if not char.isspace():
                    tokens.append(char)
        if current_token:
            tokens.append(current_token)
        
        # Process tokens to add spaces around keywords and preserve identifiers
        formatted_tokens = []
        i = 0
        while i < len(tokens):
            token = tokens[i]
            
            # Check if token is a keyword
            if token.upper() in [k.upper() for k in keywords]:
                formatted_tokens.append(token)
            # Check if token is part of a compound identifier (e.g., NUM1-VALUE)
            elif i < len(tokens) - 2 and tokens[i+1] == '-' and tokens[i+2].isalnum():
                formatted_tokens.append(f"{token}-{tokens[i+2]}")
                i += 2  # Skip the next two tokens (hyphen and second part)
            else:
                formatted_tokens.append(token)
            i += 1
        
        # Join tokens with spaces
        text_without_strings = " ".join(formatted_tokens)
        
        # Add spaces around operators
        operators = ["=", "+", "*", "/", ">", "<", ">=", "<="]
        for op in operators:
            text_without_strings = text_without_strings.replace(op, f" {op} ")
        
        # Special handling for minus/hyphen to preserve identifiers with hyphens
        text_without_strings = re.sub(r'(\s|\d)-', r'\1 - ', text_without_strings)
        text_without_strings = re.sub(r'-(\s|\d)', r' - \1', text_without_strings)
        
        # Fix spacing around identifiers with hyphens
        text_without_strings = re.sub(r'([A-Za-z0-9])\s+-\s+([A-Za-z0-9])', r'\1-\2', text_without_strings)
        
        # Clean up multiple spaces
        while "  " in text_without_strings:
            text_without_strings = text_without_strings.replace("  ", " ")
        
        # Restore string literals
        formatted_text = text_without_strings
        for i, literal in enumerate(string_literals):
            formatted_text = formatted_text.replace(f"__STR{i}__", literal)
        
        return formatted_text.strip()


def process_cobol_file(file_path, config_data, output_dir=None):
    """Process a COBOL file and extract information according to configuration"""
    try:
        # Create lexer and parser
        input_stream = FileStream(file_path, encoding='utf-8')
        lexer = Cobol85Lexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = Cobol85Parser(token_stream)
        
        # Parse the COBOL program
        tree = parser.startRule()
        
        # Create visitor and extract information
        visitor = CobolExtractorVisitor(config_data)
        result = visitor.visit(tree)
        
        # Determine output path
        if output_dir is None:
            output_dir = os.path.dirname(file_path)
        
        base_name = os.path.basename(file_path)
        file_name = os.path.splitext(base_name)[0]
        output_path = os.path.join(output_dir, f"{file_name}.json")
        
        # Write output
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(result, f, indent=2)
        
        print(f"Successfully processed {file_path}")
        print(f"JSON output saved to {output_path}")
        
        return result
    
    except Exception as e:
        print(f"Error processing {file_path}: {str(e)}")
        import traceback
        traceback.print_exc()
        return None


def process_directory(directory_path, config_data, output_dir=None, recursive=False):
    """Process all COBOL files in a directory"""
    results = {}
    
    if recursive:
        for root, dirs, files in os.walk(directory_path):
            for file in files:
                if file.endswith('.cbl') or file.endswith('.cob') or file.endswith('.cobol'):
                    file_path = os.path.join(root, file)
                    result = process_cobol_file(file_path, config_data, output_dir)
                    if result:
                        results[file_path] = result
    else:
        for filename in os.listdir(directory_path):
            if filename.endswith('.cbl') or filename.endswith('.cob') or filename.endswith('.cobol'):
                file_path = os.path.join(directory_path, filename)
                result = process_cobol_file(file_path, config_data, output_dir)
                if result:
                    results[filename] = result
    
    return results


def create_default_config():
    """Create a default YAML configuration"""
    return {
        "output_format": "json",
        "extraction": {
            "identification": {
                "enabled": True,
                "elements": ["program_id", "author", "date_written", "date_compiled", "security", "installation"]
            },
            "environment": {
                "enabled": True,
                "elements": ["source_computer", "object_computer", "special_names"]
            },
            "data": {
                "enabled": True,
                "sections": [
                    {
                        "name": "working_storage",
                        "enabled": True,
                        "extract_level_numbers": [1, 5, 10, 15, 20, 77],
                        "extract_fields": ["name", "level", "picture", "value", "usage"]
                    },
                    {
                        "name": "file_section",
                        "enabled": True,
                        "extract_fields": ["name", "level", "organization", "access_mode"]
                    },
                    {
                        "name": "linkage_section",
                        "enabled": True,
                        "extract_fields": ["name", "level", "picture"]
                    }
                ]
            },
            "procedure": {
                "enabled": True,
                "elements": ["paragraphs", "sections"],
                "statement_types": ["PERFORM", "IF", "MOVE", "COMPUTE", "CALL", "DISPLAY"],
                "extract_comments": True
            },
            "custom_analysis": {
                "enabled": True,
                "elements": ["variable_usage", "paragraph_calls", "data_dependencies"]
            }
        }
    }


def main():
    """Main function"""
    parser = argparse.ArgumentParser(description='Extract information from COBOL programs according to a YAML configuration.')
    
    parser.add_argument('--config', dest='config_file',
                        help='Path to the YAML configuration file (optional)')
    parser.add_argument('--output-dir', dest='output_dir',
                        help='Directory to save the JSON output (default: same directory as input file)')
    parser.add_argument('--recursive', dest='recursive', action='store_true',
                        help='Process directories recursively')
    parser.add_argument('input', nargs='?',
                        help='Path to the COBOL file or directory to process')
    
    args = parser.parse_args()
    
    # Check if input is provided
    if not args.input:
        parser.print_help()
        return
    
    # Load configuration
    config_data = None
    if args.config_file and os.path.isfile(args.config_file):
        with open(args.config_file, 'r') as f:
            config_data = yaml.safe_load(f)
    else:
        config_data = create_default_config()
    
    # Process input
    if os.path.isfile(args.input):
        # Process a single file
        process_cobol_file(args.input, config_data, args.output_dir)
    elif os.path.isdir(args.input):
        # Process a directory
        process_directory(args.input, config_data, args.output_dir, args.recursive)
    else:
        print(f"Error: {args.input} is not a valid file or directory")


if __name__ == "__main__":
    main()
