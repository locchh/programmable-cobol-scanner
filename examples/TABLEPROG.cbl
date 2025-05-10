       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLEPROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PRODUCT-TABLE.
           05 PRODUCT OCCURS 5 TIMES INDEXED BY PROD-IDX.
               10 PRODUCT-ID       PIC 9(3).
               10 PRODUCT-NAME     PIC X(20).
               10 PRODUCT-PRICE    PIC 9(5)V99.
       
       01 COUNTER                  PIC 9(2) VALUE 1.
       01 TOTAL-PRICE              PIC 9(7)V99 VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INITIALIZE-TABLE.
           PERFORM DISPLAY-TABLE.
           PERFORM CALCULATE-TOTAL.
           
           DISPLAY "TOTAL PRICE OF ALL PRODUCTS: " TOTAL-PRICE.
           STOP RUN.
           
       INITIALIZE-TABLE.
           MOVE 101 TO PRODUCT-ID (1).
           MOVE "LAPTOP" TO PRODUCT-NAME (1).
           MOVE 1200.00 TO PRODUCT-PRICE (1).
           
           MOVE 102 TO PRODUCT-ID (2).
           MOVE "SMARTPHONE" TO PRODUCT-NAME (2).
           MOVE 800.50 TO PRODUCT-PRICE (2).
           
           MOVE 103 TO PRODUCT-ID (3).
           MOVE "TABLET" TO PRODUCT-NAME (3).
           MOVE 500.75 TO PRODUCT-PRICE (3).
           
           MOVE 104 TO PRODUCT-ID (4).
           MOVE "MONITOR" TO PRODUCT-NAME (4).
           MOVE 350.25 TO PRODUCT-PRICE (4).
           
           MOVE 105 TO PRODUCT-ID (5).
           MOVE "KEYBOARD" TO PRODUCT-NAME (5).
           MOVE 75.99 TO PRODUCT-PRICE (5).
           
       DISPLAY-TABLE.
           PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 5
               DISPLAY "PRODUCT: " PRODUCT-ID (COUNTER) 
                       " - " PRODUCT-NAME (COUNTER)
                       " - $" PRODUCT-PRICE (COUNTER)
           END-PERFORM.
           
       CALCULATE-TOTAL.
           PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 5
               ADD PRODUCT-PRICE (COUNTER) TO TOTAL-PRICE
           END-PERFORM.