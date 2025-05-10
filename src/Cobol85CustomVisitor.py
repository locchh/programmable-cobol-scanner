#!/usr/bin/env python3
import re
from antlr4 import *
from Cobol85Visitor import Cobol85Visitor
from Cobol85Parser import Cobol85Parser


class Cobol85CustomVisitor(Cobol85Visitor):
    """
    A custom visitor for COBOL programs that extracts information and exports it to JSON.
    """
    def __init__(self):
        super().__init__()
        self.program_info = {
            "identification": {},
            "environment": {},
            "data": {
                "working_storage": [],
                "file_section": [],
                "linkage_section": []
            },
            "procedure": {
                "paragraphs": {}
            }
        }
        self.current_division = None
        self.current_section = None
        self.current_paragraph = None
        self.current_data_item = None
        self.current_level = None
    
    def _format_statement(self, text):
        """
        Format a statement text to improve readability.
        """
        # Common COBOL keywords that should have spaces after them
        keywords = [
            "ADD", "COMPUTE", "DISPLAY", "MOVE", "PERFORM", "UNTIL", "TO", "STOP", "RUN",
            "IF", "ELSE", "END-IF", "EVALUATE", "WHEN", "END-EVALUATE", "VARYING", "FROM",
            "BY", "CALL", "USING", "OPEN", "CLOSE", "READ", "WRITE", "REWRITE", "DELETE",
            "START", "INITIALIZE", "ACCEPT", "STRING", "UNSTRING", "INSPECT", "REPLACING"
        ]
        
        # Insert spaces between keywords and identifiers
        for keyword in keywords:
            # Replace the keyword followed by non-space with keyword + space
            pattern = f"({keyword})([^ ])"
            text = re.sub(pattern, r"\1 \2", text)
        
        # Handle operators
        operators = ["=", ">", "<", "+", "-", "*", "/"]
        for op in operators:
            # Don't replace hyphens in identifiers
            if op != "-":
                text = text.replace(op, f" {op} ")
            else:
                # Special handling for minus/hyphen
                # Replace only when it's an operator (surrounded by spaces or digits)
                text = re.sub(r'(\s|\d)-(\s|\d)', r'\1 - \2', text)
        
        # Handle quotes and string literals
        parts = []
        in_quotes = False
        current = ""
        i = 0
        
        while i < len(text):
            if text[i:i+1] == '"':
                if not in_quotes:  # Start of quoted string
                    if current:  # Save accumulated text
                        parts.append(current)
                        current = ""
                    parts.append('"')  # Add opening quote
                    in_quotes = True
                else:  # End of quoted string
                    parts.append('"')  # Add closing quote
                    in_quotes = False
                i += 1
            elif in_quotes:  # Inside quoted string, preserve as is
                parts.append(text[i:i+1])
                i += 1
            else:  # Outside quoted string
                current += text[i:i+1]
                i += 1
        
        if current:  # Add any remaining text
            parts.append(current)
        
        # Reconstruct with proper spacing
        result = ""
        for i, part in enumerate(parts):
            if part == '"':  # Quote mark
                result += part
            elif i > 0 and parts[i-1] == '"' and part != '"':  # Text after opening quote
                result += part
            else:  # Regular text
                result += part
        
        # Fix spacing around identifiers with hyphens
        result = re.sub(r'([A-Za-z0-9])\s*-\s*([A-Za-z0-9])', r'\1-\2', result)
        
        # Clean up multiple spaces
        while "  " in result:
            result = result.replace("  ", " ")
        
        return result.strip()
    
    def visitStartRule(self, ctx:Cobol85Parser.StartRuleContext):
        self.visitChildren(ctx)
        return self.program_info
    
    def visitIdentificationDivision(self, ctx:Cobol85Parser.IdentificationDivisionContext):
        self.current_division = "identification"
        return self.visitChildren(ctx)
    
    def visitProgramIdParagraph(self, ctx:Cobol85Parser.ProgramIdParagraphContext):
        if ctx.programName():
            program_name = ctx.programName().getText()
            self.program_info["identification"]["program_id"] = program_name
        return self.visitChildren(ctx)
    
    def visitAuthorParagraph(self, ctx:Cobol85Parser.AuthorParagraphContext):
        if ctx.getChildCount() > 2:  # AUTHOR. <name>
            author = ctx.getChild(2).getText()
            self.program_info["identification"]["author"] = author
        return self.visitChildren(ctx)
    
    def visitDateWrittenParagraph(self, ctx:Cobol85Parser.DateWrittenParagraphContext):
        if ctx.getChildCount() > 2:  # DATE-WRITTEN. <date>
            date_written = ctx.getChild(2).getText()
            self.program_info["identification"]["date_written"] = date_written
        return self.visitChildren(ctx)
    
    def visitSecurityParagraph(self, ctx:Cobol85Parser.SecurityParagraphContext):
        if ctx.getChildCount() > 2:  # SECURITY. <level>
            security = ctx.getChild(2).getText()
            self.program_info["identification"]["security"] = security
        return self.visitChildren(ctx)
    
    def visitEnvironmentDivision(self, ctx:Cobol85Parser.EnvironmentDivisionContext):
        self.current_division = "environment"
        return self.visitChildren(ctx)
    
    def visitSourceComputerParagraph(self, ctx:Cobol85Parser.SourceComputerParagraphContext):
        if ctx.computerName():
            computer_name = ctx.computerName().getText()
            self.program_info["environment"]["source_computer"] = computer_name
        return self.visitChildren(ctx)
    
    def visitObjectComputerParagraph(self, ctx:Cobol85Parser.ObjectComputerParagraphContext):
        if ctx.computerName():
            computer_name = ctx.computerName().getText()
            self.program_info["environment"]["object_computer"] = computer_name
        return self.visitChildren(ctx)
    
    def visitDataDivision(self, ctx:Cobol85Parser.DataDivisionContext):
        self.current_division = "data"
        return self.visitChildren(ctx)
    
    def visitWorkingStorageSection(self, ctx:Cobol85Parser.WorkingStorageSectionContext):
        self.current_section = "working_storage"
        result = self.visitChildren(ctx)
        self.current_section = None
        return result
    
    def visitFileSection(self, ctx:Cobol85Parser.FileSectionContext):
        self.current_section = "file_section"
        result = self.visitChildren(ctx)
        self.current_section = None
        return result
    
    def visitLinkageSection(self, ctx:Cobol85Parser.LinkageSectionContext):
        self.current_section = "linkage_section"
        result = self.visitChildren(ctx)
        self.current_section = None
        return result
    
    def visitDataDescriptionEntryFormat1(self, ctx:Cobol85Parser.DataDescriptionEntryFormat1Context):
        if self.current_section:
            data_item = {}
            
            # Get level number
            if ctx.INTEGERLITERAL():
                level = ctx.INTEGERLITERAL().getText()
                data_item["level"] = level
            elif ctx.LEVEL_NUMBER_77():
                data_item["level"] = "77"
            
            # Get data name
            if ctx.dataName():
                name = ctx.dataName().getText()
                data_item["name"] = name
            elif ctx.FILLER():
                data_item["name"] = "FILLER"
            
            # Process clauses
            for i in range(ctx.getChildCount()):
                child = ctx.getChild(i)
                
                # Picture clause
                if isinstance(child, Cobol85Parser.DataPictureClauseContext):
                    if child.pictureString():
                        pic = child.pictureString().getText()
                        data_item["picture"] = pic.replace("PIC", "").strip()
                
                # Value clause
                elif isinstance(child, Cobol85Parser.DataValueClauseContext):
                    # Extract value from the context
                    value_text = child.getText()
                    # Remove 'VALUE' and 'IS' keywords
                    value_text = value_text.replace("VALUE", "").replace("IS", "").strip()
                    # Remove quotes if present
                    if value_text.startswith('"') and value_text.endswith('"'):
                        value_text = value_text[1:-1]
                    data_item["value"] = value_text
                
                # Usage clause
                elif isinstance(child, Cobol85Parser.DataUsageClauseContext):
                    usage = child.getText().replace("USAGE", "").replace("IS", "").strip()
                    data_item["usage"] = usage
            
            # Add data item to the appropriate section
            self.program_info["data"][self.current_section].append(data_item)
        
        return self.visitChildren(ctx)
    
    def visitProcedureDivision(self, ctx:Cobol85Parser.ProcedureDivisionContext):
        self.current_division = "procedure"
        return self.visitChildren(ctx)
    
    def visitParagraph(self, ctx:Cobol85Parser.ParagraphContext):
        if ctx.paragraphName():
            paragraph_name = ctx.paragraphName().getText()
            self.current_paragraph = paragraph_name
            self.program_info["procedure"]["paragraphs"][paragraph_name] = {
                "statements": []
            }
            
            # Process sentences
            for i in range(ctx.getChildCount()):
                child = ctx.getChild(i)
                if isinstance(child, Cobol85Parser.SentenceContext):
                    for j in range(child.getChildCount()):
                        stmt = child.getChild(j)
                        if isinstance(stmt, Cobol85Parser.StatementContext):
                            # Get the original text from the token stream
                            if stmt.start and stmt.stop and hasattr(stmt.start, 'tokenSource'):
                                token_source = stmt.start.tokenSource
                                input_stream = token_source.inputStream
                                start = stmt.start.start
                                stop = stmt.stop.stop
                                if input_stream and start is not None and stop is not None:
                                    stmt_text = input_stream.getText(start, stop)
                                else:
                                    # Fallback to regular getText with formatting improvements
                                    stmt_text = self._format_statement(stmt.getText())
                            else:
                                # Fallback to regular getText with formatting improvements
                                stmt_text = self._format_statement(stmt.getText())
                            
                            self.program_info["procedure"]["paragraphs"][paragraph_name]["statements"].append(stmt_text)
        
        result = self.visitChildren(ctx)
        self.current_paragraph = None
        return result