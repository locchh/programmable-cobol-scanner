       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPLEX.
       AUTHOR. CASCADE.
       DATE-WRITTEN. 2025-05-10.
       SECURITY. CONFIDENTIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. X86-64.
       OBJECT-COMPUTER. X86-64.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "EMPLOYEE.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.
           SELECT REPORT-FILE ASSIGN TO "REPORT.OUT"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE
           LABEL RECORDS ARE STANDARD.
       01 EMPLOYEE-RECORD.
           05 EMPLOYEE-ID       PIC 9(5).
           05 EMPLOYEE-NAME     PIC X(20).
           05 EMPLOYEE-DEPT     PIC X(10).
           05 EMPLOYEE-SALARY   PIC 9(7)V99.
           05 FILLER            PIC X(36).

       FD REPORT-FILE
           LABEL RECORDS ARE STANDARD.
       01 REPORT-LINE           PIC X(80).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS           PIC XX VALUE SPACES.
       01 WS-EOF                PIC X VALUE "N".
       01 WS-TOTAL-SALARY       PIC 9(9)V99 VALUE ZERO.
       01 WS-EMPLOYEE-COUNT     PIC 9(5) VALUE ZERO.

       01 HEADER-1.
           05 FILLER            PIC X(20) VALUE "EMPLOYEE REPORT".
           05 FILLER            PIC X(60) VALUE SPACES.

       01 HEADER-2.
           05 FILLER            PIC X(5) VALUE "ID".
           05 FILLER            PIC X(3) VALUE SPACES.
           05 FILLER            PIC X(10) VALUE "NAME".
           05 FILLER            PIC X(3) VALUE SPACES.
           05 FILLER            PIC X(10) VALUE "DEPARTMENT".
           05 FILLER            PIC X(3) VALUE SPACES.
           05 FILLER            PIC X(10) VALUE "SALARY".
           05 FILLER            PIC X(36) VALUE SPACES.

       01 DETAIL-LINE.
           05 DL-ID             PIC 9(5).
           05 FILLER            PIC X(3) VALUE SPACES.
           05 DL-NAME           PIC X(20).
           05 FILLER            PIC X(3) VALUE SPACES.
           05 DL-DEPT           PIC X(10).
           05 FILLER            PIC X(3) VALUE SPACES.
           05 DL-SALARY         PIC $$$,$$$,$$9.99.
           05 FILLER            PIC X(16) VALUE SPACES.

       01 TOTAL-LINE.
           05 FILLER            PIC X(20) VALUE "TOTAL SALARY: ".
           05 TL-TOTAL          PIC $$$,$$$,$$9.99.
           05 FILLER            PIC X(10) VALUE SPACES.
           05 FILLER            PIC X(15) VALUE "EMPLOYEE COUNT: ".
           05 TL-COUNT          PIC ZZ,ZZ9.
           05 FILLER            PIC X(15) VALUE SPACES.

       01 DEPARTMENT-TABLE.
           05 DEPT-ENTRY OCCURS 5 TIMES INDEXED BY DEPT-IDX.
               10 DEPT-ID       PIC 9(3).
               10 DEPT-NAME     PIC X(15).
               10 DEPT-MANAGER  PIC X(20).
               10 DEPT-BUDGET   PIC 9(7)V99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INITIALIZE-DEPT-TABLE.
           OPEN INPUT EMPLOYEE-FILE
                OUTPUT REPORT-FILE.

           IF FILE-STATUS NOT = "00"
               DISPLAY "ERROR OPENING FILE: " FILE-STATUS
               STOP RUN
           END-IF.

           WRITE REPORT-LINE FROM HEADER-1.
           WRITE REPORT-LINE FROM HEADER-2.

           PERFORM READ-EMPLOYEE.
           PERFORM PROCESS-EMPLOYEE UNTIL WS-EOF = "Y".

           MOVE WS-TOTAL-SALARY TO TL-TOTAL.
           MOVE WS-EMPLOYEE-COUNT TO TL-COUNT.
           WRITE REPORT-LINE FROM TOTAL-LINE.

           CLOSE EMPLOYEE-FILE
                 REPORT-FILE.
           STOP RUN.

       INITIALIZE-DEPT-TABLE.
           MOVE 100 TO DEPT-ID (1).
           MOVE "ENGINEERING" TO DEPT-NAME (1).
           MOVE "JOHN SMITH" TO DEPT-MANAGER (1).
           MOVE 1000000.00 TO DEPT-BUDGET (1).

           MOVE 200 TO DEPT-ID (2).
           MOVE "MARKETING" TO DEPT-NAME (2).
           MOVE "JANE DOE" TO DEPT-MANAGER (2).
           MOVE 750000.00 TO DEPT-BUDGET (2).

           MOVE 300 TO DEPT-ID (3).
           MOVE "FINANCE" TO DEPT-NAME (3).
           MOVE "BOB JOHNSON" TO DEPT-MANAGER (3).
           MOVE 500000.00 TO DEPT-BUDGET (3).

           MOVE 400 TO DEPT-ID (4).
           MOVE "HR" TO DEPT-NAME (4).
           MOVE "ALICE BROWN" TO DEPT-MANAGER (4).
           MOVE 300000.00 TO DEPT-BUDGET (4).

           MOVE 500 TO DEPT-ID (5).
           MOVE "OPERATIONS" TO DEPT-NAME (5).
           MOVE "CHARLIE GREEN" TO DEPT-MANAGER (5).
           MOVE 1200000.00 TO DEPT-BUDGET (5).

       READ-EMPLOYEE.
           READ EMPLOYEE-FILE
               AT END MOVE "Y" TO WS-EOF
           END-READ.

       PROCESS-EMPLOYEE.
           ADD 1 TO WS-EMPLOYEE-COUNT.
           ADD EMPLOYEE-SALARY TO WS-TOTAL-SALARY.

           MOVE EMPLOYEE-ID TO DL-ID.
           MOVE EMPLOYEE-NAME TO DL-NAME.
           MOVE EMPLOYEE-DEPT TO DL-DEPT.
           MOVE EMPLOYEE-SALARY TO DL-SALARY.

           WRITE REPORT-LINE FROM DETAIL-LINE.

           PERFORM READ-EMPLOYEE.