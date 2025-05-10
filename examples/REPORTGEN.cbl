       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTGEN.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. X86-64.
       OBJECT-COMPUTER. X86-64.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE ASSIGN TO "SALES.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO "REPORT.OUT"
               ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD SALES-FILE
           LABEL RECORDS ARE STANDARD.
       01 SALES-RECORD.
           05 SALES-ID         PIC 9(5).
           05 SALES-PERSON     PIC X(20).
           05 SALES-AMOUNT     PIC 9(7)V99.
           05 SALES-DATE       PIC X(10).
       
       FD REPORT-FILE
           LABEL RECORDS ARE STANDARD.
       01 REPORT-LINE          PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-EOF               PIC X VALUE "N".
       01 WS-TOTAL-SALES       PIC 9(9)V99 VALUE 0.
       01 WS-RECORD-COUNT      PIC 9(5) VALUE 0.
       
       01 HEADER-1.
           05 FILLER           PIC X(20) VALUE "SALES REPORT".
           05 FILLER           PIC X(60) VALUE SPACES.
       
       01 HEADER-2.
           05 FILLER           PIC X(5) VALUE "ID".
           05 FILLER           PIC X(3) VALUE SPACES.
           05 FILLER           PIC X(20) VALUE "SALESPERSON".
           05 FILLER           PIC X(3) VALUE SPACES.
           05 FILLER           PIC X(10) VALUE "AMOUNT".
           05 FILLER           PIC X(3) VALUE SPACES.
           05 FILLER           PIC X(10) VALUE "DATE".
           05 FILLER           PIC X(26) VALUE SPACES.
       
       01 DETAIL-LINE.
           05 DL-ID            PIC 9(5).
           05 FILLER           PIC X(3) VALUE SPACES.
           05 DL-NAME          PIC X(20).
           05 FILLER           PIC X(3) VALUE SPACES.
           05 DL-AMOUNT        PIC $$$,$$$,$$9.99.
           05 FILLER           PIC X(3) VALUE SPACES.
           05 DL-DATE          PIC X(10).
           05 FILLER           PIC X(16) VALUE SPACES.
       
       01 TOTAL-LINE.
           05 FILLER           PIC X(20) VALUE "TOTAL SALES: ".
           05 TL-TOTAL         PIC $$$,$$$,$$9.99.
           05 FILLER           PIC X(48) VALUE SPACES.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT SALES-FILE
                OUTPUT REPORT-FILE.
           
           WRITE REPORT-LINE FROM HEADER-1.
           WRITE REPORT-LINE FROM HEADER-2.
           
           PERFORM READ-RECORD.
           PERFORM PROCESS-RECORD UNTIL WS-EOF = "Y".
           
           MOVE WS-TOTAL-SALES TO TL-TOTAL.
           WRITE REPORT-LINE FROM TOTAL-LINE.
           
           CLOSE SALES-FILE
                 REPORT-FILE.
           STOP RUN.
           
       READ-RECORD.
           READ SALES-FILE
               AT END MOVE "Y" TO WS-EOF
           END-READ.
           
       PROCESS-RECORD.
           ADD 1 TO WS-RECORD-COUNT.
           MOVE SALES-ID TO DL-ID.
           MOVE SALES-PERSON TO DL-NAME.
           MOVE SALES-AMOUNT TO DL-AMOUNT.
           MOVE SALES-DATE TO DL-DATE.
           
           WRITE REPORT-LINE FROM DETAIL-LINE.
           
           ADD SALES-AMOUNT TO WS-TOTAL-SALES.
           PERFORM READ-RECORD.