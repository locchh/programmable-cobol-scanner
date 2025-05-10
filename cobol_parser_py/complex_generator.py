#!/usr/bin/env python3
import sys
import os
from antlr4 import *
from antlr4.error.ErrorListener import ErrorListener
from Cobol85Lexer import Cobol85Lexer
from Cobol85Parser import Cobol85Parser
from Cobol85Listener import Cobol85Listener

def generate_complex_cobol_program(program_name):
    """
    Generate a more complex COBOL program that demonstrates various features
    of the COBOL 85 grammar.
    """
    program = []
    
    # Identification Division
    program.append("       IDENTIFICATION DIVISION.")
    program.append(f"       PROGRAM-ID. {program_name}.")
    program.append("       AUTHOR. CASCADE.")
    program.append("       DATE-WRITTEN. 2025-05-10.")
    program.append("       SECURITY. CONFIDENTIAL.")
    program.append("")
    
    # Environment Division
    program.append("       ENVIRONMENT DIVISION.")
    program.append("       CONFIGURATION SECTION.")
    program.append("       SOURCE-COMPUTER. X86-64.")
    program.append("       OBJECT-COMPUTER. X86-64.")
    program.append("")
    program.append("       INPUT-OUTPUT SECTION.")
    program.append("       FILE-CONTROL.")
    program.append("           SELECT EMPLOYEE-FILE ASSIGN TO \"EMPLOYEE.DAT\"")
    program.append("               ORGANIZATION IS SEQUENTIAL")
    program.append("               ACCESS MODE IS SEQUENTIAL")
    program.append("               FILE STATUS IS FILE-STATUS.")
    program.append("           SELECT REPORT-FILE ASSIGN TO \"REPORT.OUT\"")
    program.append("               ORGANIZATION IS SEQUENTIAL.")
    program.append("")
    
    # Data Division
    program.append("       DATA DIVISION.")
    program.append("       FILE SECTION.")
    program.append("       FD EMPLOYEE-FILE")
    program.append("           LABEL RECORDS ARE STANDARD.")
    program.append("       01 EMPLOYEE-RECORD.")
    program.append("           05 EMPLOYEE-ID       PIC 9(5).")
    program.append("           05 EMPLOYEE-NAME     PIC X(20).")
    program.append("           05 EMPLOYEE-DEPT     PIC X(10).")
    program.append("           05 EMPLOYEE-SALARY   PIC 9(7)V99.")
    program.append("           05 FILLER            PIC X(36).")
    program.append("")
    program.append("       FD REPORT-FILE")
    program.append("           LABEL RECORDS ARE STANDARD.")
    program.append("       01 REPORT-LINE           PIC X(80).")
    program.append("")
    program.append("       WORKING-STORAGE SECTION.")
    program.append("       01 FILE-STATUS           PIC XX VALUE SPACES.")
    program.append("       01 WS-EOF                PIC X VALUE \"N\".")
    program.append("       01 WS-TOTAL-SALARY       PIC 9(9)V99 VALUE ZERO.")
    program.append("       01 WS-EMPLOYEE-COUNT     PIC 9(5) VALUE ZERO.")
    program.append("")
    program.append("       01 HEADER-1.")
    program.append("           05 FILLER            PIC X(20) VALUE \"EMPLOYEE REPORT\".")
    program.append("           05 FILLER            PIC X(60) VALUE SPACES.")
    program.append("")
    program.append("       01 HEADER-2.")
    program.append("           05 FILLER            PIC X(5) VALUE \"ID\".")
    program.append("           05 FILLER            PIC X(3) VALUE SPACES.")
    program.append("           05 FILLER            PIC X(10) VALUE \"NAME\".")
    program.append("           05 FILLER            PIC X(3) VALUE SPACES.")
    program.append("           05 FILLER            PIC X(10) VALUE \"DEPARTMENT\".")
    program.append("           05 FILLER            PIC X(3) VALUE SPACES.")
    program.append("           05 FILLER            PIC X(10) VALUE \"SALARY\".")
    program.append("           05 FILLER            PIC X(36) VALUE SPACES.")
    program.append("")
    program.append("       01 DETAIL-LINE.")
    program.append("           05 DL-ID             PIC 9(5).")
    program.append("           05 FILLER            PIC X(3) VALUE SPACES.")
    program.append("           05 DL-NAME           PIC X(20).")
    program.append("           05 FILLER            PIC X(3) VALUE SPACES.")
    program.append("           05 DL-DEPT           PIC X(10).")
    program.append("           05 FILLER            PIC X(3) VALUE SPACES.")
    program.append("           05 DL-SALARY         PIC $$$,$$$,$$9.99.")
    program.append("           05 FILLER            PIC X(16) VALUE SPACES.")
    program.append("")
    program.append("       01 TOTAL-LINE.")
    program.append("           05 FILLER            PIC X(20) VALUE \"TOTAL SALARY: \".")
    program.append("           05 TL-TOTAL          PIC $$$,$$$,$$9.99.")
    program.append("           05 FILLER            PIC X(10) VALUE SPACES.")
    program.append("           05 FILLER            PIC X(15) VALUE \"EMPLOYEE COUNT: \".")
    program.append("           05 TL-COUNT          PIC ZZ,ZZ9.")
    program.append("           05 FILLER            PIC X(15) VALUE SPACES.")
    program.append("")
    program.append("       01 DEPARTMENT-TABLE.")
    program.append("           05 DEPT-ENTRY OCCURS 5 TIMES INDEXED BY DEPT-IDX.")
    program.append("               10 DEPT-ID       PIC 9(3).")
    program.append("               10 DEPT-NAME     PIC X(15).")
    program.append("               10 DEPT-MANAGER  PIC X(20).")
    program.append("               10 DEPT-BUDGET   PIC 9(7)V99.")
    program.append("")
    
    # Procedure Division
    program.append("       PROCEDURE DIVISION.")
    program.append("       MAIN-PARA.")
    program.append("           PERFORM INITIALIZE-DEPT-TABLE.")
    program.append("           OPEN INPUT EMPLOYEE-FILE")
    program.append("                OUTPUT REPORT-FILE.")
    program.append("")
    program.append("           IF FILE-STATUS NOT = \"00\"")
    program.append("               DISPLAY \"ERROR OPENING FILE: \" FILE-STATUS")
    program.append("               STOP RUN")
    program.append("           END-IF.")
    program.append("")
    program.append("           WRITE REPORT-LINE FROM HEADER-1.")
    program.append("           WRITE REPORT-LINE FROM HEADER-2.")
    program.append("")
    program.append("           PERFORM READ-EMPLOYEE.")
    program.append("           PERFORM PROCESS-EMPLOYEE UNTIL WS-EOF = \"Y\".")
    program.append("")
    program.append("           MOVE WS-TOTAL-SALARY TO TL-TOTAL.")
    program.append("           MOVE WS-EMPLOYEE-COUNT TO TL-COUNT.")
    program.append("           WRITE REPORT-LINE FROM TOTAL-LINE.")
    program.append("")
    program.append("           CLOSE EMPLOYEE-FILE")
    program.append("                 REPORT-FILE.")
    program.append("           STOP RUN.")
    program.append("")
    program.append("       INITIALIZE-DEPT-TABLE.")
    program.append("           MOVE 100 TO DEPT-ID (1).")
    program.append("           MOVE \"ENGINEERING\" TO DEPT-NAME (1).")
    program.append("           MOVE \"JOHN SMITH\" TO DEPT-MANAGER (1).")
    program.append("           MOVE 1000000.00 TO DEPT-BUDGET (1).")
    program.append("")
    program.append("           MOVE 200 TO DEPT-ID (2).")
    program.append("           MOVE \"MARKETING\" TO DEPT-NAME (2).")
    program.append("           MOVE \"JANE DOE\" TO DEPT-MANAGER (2).")
    program.append("           MOVE 750000.00 TO DEPT-BUDGET (2).")
    program.append("")
    program.append("           MOVE 300 TO DEPT-ID (3).")
    program.append("           MOVE \"FINANCE\" TO DEPT-NAME (3).")
    program.append("           MOVE \"BOB JOHNSON\" TO DEPT-MANAGER (3).")
    program.append("           MOVE 500000.00 TO DEPT-BUDGET (3).")
    program.append("")
    program.append("           MOVE 400 TO DEPT-ID (4).")
    program.append("           MOVE \"HR\" TO DEPT-NAME (4).")
    program.append("           MOVE \"ALICE BROWN\" TO DEPT-MANAGER (4).")
    program.append("           MOVE 300000.00 TO DEPT-BUDGET (4).")
    program.append("")
    program.append("           MOVE 500 TO DEPT-ID (5).")
    program.append("           MOVE \"OPERATIONS\" TO DEPT-NAME (5).")
    program.append("           MOVE \"CHARLIE GREEN\" TO DEPT-MANAGER (5).")
    program.append("           MOVE 1200000.00 TO DEPT-BUDGET (5).")
    program.append("")
    program.append("       READ-EMPLOYEE.")
    program.append("           READ EMPLOYEE-FILE")
    program.append("               AT END MOVE \"Y\" TO WS-EOF")
    program.append("           END-READ.")
    program.append("")
    program.append("       PROCESS-EMPLOYEE.")
    program.append("           ADD 1 TO WS-EMPLOYEE-COUNT.")
    program.append("           ADD EMPLOYEE-SALARY TO WS-TOTAL-SALARY.")
    program.append("")
    program.append("           MOVE EMPLOYEE-ID TO DL-ID.")
    program.append("           MOVE EMPLOYEE-NAME TO DL-NAME.")
    program.append("           MOVE EMPLOYEE-DEPT TO DL-DEPT.")
    program.append("           MOVE EMPLOYEE-SALARY TO DL-SALARY.")
    program.append("")
    program.append("           WRITE REPORT-LINE FROM DETAIL-LINE.")
    program.append("")
    program.append("           PERFORM READ-EMPLOYEE.")
    
    return "\n".join(program)

def main():
    if len(sys.argv) > 1:
        program_name = sys.argv[1]
    else:
        program_name = "COMPLEX"
    
    output_file = f"/home/loc/Works/untitled/examples/{program_name}.cbl"
    
    print(f"Generating complex COBOL program: {program_name}")
    cobol_code = generate_complex_cobol_program(program_name)
    
    with open(output_file, 'w') as f:
        f.write(cobol_code)
    
    print(f"Complex COBOL program saved to {output_file}")
    
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
        return True
    except Exception as e:
        print(f"\nError parsing generated code: {str(e)}")
        return False

if __name__ == '__main__':
    main()
