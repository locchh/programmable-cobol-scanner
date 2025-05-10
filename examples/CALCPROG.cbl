       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCPROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1                  PIC 9(5) VALUE 25.
       01 NUM2                  PIC 9(5) VALUE 10.
       01 RESULT                PIC 9(6).
       01 OPERATION             PIC X.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "ENTER OPERATION (+, -, *, /): ".
           ACCEPT OPERATION.
           
           EVALUATE OPERATION
               WHEN "+"
                   ADD NUM1 TO NUM2 GIVING RESULT
               WHEN "-"
                   SUBTRACT NUM2 FROM NUM1 GIVING RESULT
               WHEN "*"
                   MULTIPLY NUM1 BY NUM2 GIVING RESULT
               WHEN "/"
                   DIVIDE NUM1 BY NUM2 GIVING RESULT
                   ON SIZE ERROR
                       DISPLAY "DIVISION ERROR"
                   NOT ON SIZE ERROR
                       DISPLAY "RESULT IS: " RESULT
                   END-DIVIDE
               WHEN OTHER
                   DISPLAY "INVALID OPERATION"
           END-EVALUATE.
           
           IF RESULT > 1000
               DISPLAY "RESULT IS GREATER THAN 1000"
           ELSE
               DISPLAY "RESULT IS LESS THAN OR EQUAL TO 1000"
           END-IF.
           
           STOP RUN.