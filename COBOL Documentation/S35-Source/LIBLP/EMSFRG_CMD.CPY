      * COPYMEMBER: LIBLP/EMSFRG.CMD
       01  LEGEND-BR-01.
           03  FILLER           PIC X(49)  VALUE
               "F1-PRINTER, F7-EXIT, v, ENTER BEGINNING BRANCH NO".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".
       01  LEGEND-BR-01-A.
           03  FILLER           PIC X(51)  VALUE
               "F1-PRINTER, F7-EXIT, v, ENTER BEGINNING BRANCH NO.:".
       01  LEGEND-BR-02.
           03  FILLER           PIC X(31)  VALUE
               "^, v, ENTER ENDING BRANCH NO. :".
       01  LEGEND-CL-01.
           03  FILLER           PIC X(40)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING CLASS NO".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".
       01  LEGEND-CL-02.
           03  FILLER           PIC X(31)  VALUE
               "^, v, ENTER ENDING CLASS NO. : ".
       01  LEGEND-SEQ-01.
           03  FILLER           PIC X(40)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING SEQUENCE".
           03  FILLER           PIC X(11)  VALUE
               "  (F3-ALL):".
       01  LEGEND-SEQ-02.
           03  FILLER           PIC X(31)  VALUE
               "^, v, ENTER ENDING SEQUENCE: ".
       01  LEGEND-GTT-01.
           03  FILLER           PIC X(49)  VALUE
               " F7-EXIT, ^, v, ENTER (I)NCLUDE, (S)KIP, (O)NLY :".
       01  LEGEND-PL-01.
           03  FILLER           PIC X(39)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING POOL NO".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".
       01  LEGEND-PL-02.
           03  FILLER           PIC X(30)  VALUE
               "^, v, ENTER ENDING POOL NO. : ".
       01  LEGEND-INCLDETAIL.
           03  FILLER           PIC X(38)  VALUE
               "^, v, INCLUDE DETAIL LINES ON REPORT?:".
       01  LEGEND-NOTES-ISO.
           03  FILLER           PIC X(49)  VALUE
               " F7-EXIT, ^, v, ENTER (I)NCLUDE, (S)KIP, (O)NLY :".
       01  LEGEND-DTE.
           03  FILLER           PIC X(38)  VALUE
               "F7-EXIT, ^, v, ENTER MONTH ENDING DATE".
           03  FILLER           PIC X(8) VALUE
               " (MMYY):".
       01  LEGEND-TRANS-DTE.
           03  FILLER           PIC X(37)  VALUE
               "F7-EXIT, ^, v, ENTER TRANSACTION DATE".
           03  FILLER           PIC X(10) VALUE
               " (MMDDYY):".
       01  LEGEND-DA-01.
           03  FILLER           PIC X(43)  VALUE
               "F7-EXIT, ^, v, ENTER DAY OF MONTH (F3-ALL):".
       01  LEGEND-DA-02.
           03  FILLER           PIC X(41)  VALUE
               "F7-EXIT, ^, v, ENTER ENDING DAY OF MONTH:".
       01  LEGEND-GLNO-01.
           03  FILLER           PIC X(50)  VALUE
               "F7-EXIT, ^, v, ENTER BEGINNING GL ACCTNO (F3-ALL):".
       01  LEGEND-GLNO-02.
           03  FILLER           PIC X(38)  VALUE
               "F7-EXIT, ^, v, ENTER ENDING GL ACCTNO:".
       01  legend-printcol-01.
           03  filler           pic x(47)  value
               "PRINT COLLATERAL INFORMATION WITH REPORT (Y/N):".
       01  LEGEND-LOCAL-SPOOL.
           03  filler           pic x(31)  value
               "SPOOL TO LOCAL DIRECTORY (Y/N):".
       01  LEGEND-ACCEPT.
           03  FILLER           PIC X(40)  VALUE
               "F7-EXIT, F1, ^, ACCEPT (Y/N):".
       01  LEGEND-DRAWER-NO.
           03  FILLER           PIC X(40)  VALUE
               " F7-EXIT, ^, v, ENTER CASH DRAWER NO. (F".
           03  FILLER           PIC X(11)  VALUE
               "3-ALL):    ".

       01  LEGEND-REFNO.
           03  FILLER           PIC X(40)  VALUE
               " F7-EXIT, ^, v, ENTER REFNO, (F3-ALL)".    
       01  LEGEND-INTERBRANCH.
           03  FILLER           PIC X(45)  VALUE
               " F7-EXIT, ^, v, INTER BRANCH PMTS: (I)NCLUDE,".
           03  FILLER           PIC X(17)  VALUE
               " (S)KIP OR (O)NLY".
       01  LEGEND-DRAWER-TOTALS.
           03  FILLER           PIC X(42)  VALUE
               " F7-EXIT, ^, v, DRAWER TOTALS: Y(ES), N(O)".                 
       01  LEGEND-SUMMARY-ONLY. 
           03  FILLER           PIC X(43)  VALUE
               " F7-EXIT, ^, v, SUMMARY TOTALS: Y(ES), N(O)".                 
       01  LEGEND-INCLUDE-USER. 
           03  FILLER           PIC X(48)  VALUE
               " F7-EXIT, ^, v, ENTER USERID TO INCLUDE (F3-ALL)".            
       01  LEGEND-SKIP-PAGE-BREAKS.
           03  FILLER           PIC X(55)  VALUE
           " F7-EXIT, ^, v, SKIP PAGE BREAK ON SUMMARY: Y(ES), N(O)". 
       01  LEGEND-REVERSAL-ONLY.    
           03  FILLER           PIC X(55)  VALUE
           " F7-EXIT, ^, v, REVERSALS ONLY: Y / N                  ".
       01  LEGEND-CREDIT-01.
           03  FILLER           PIC X(41)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING HI CREDIT".
           03  FILLER           PIC X(11)  VALUE
               "  (F3-ALL):".
       01  LEGEND-CREDIT-02.
           03  FILLER           PIC X(31)  VALUE
               "^, v, ENTER ENDING HI CREDIT : ".
       01  LEGEND-ISO.
           03  FILLER           PIC X(56)  VALUE
             "^, v, ENTER 'I' TO INCLUDE, 'S' TO SKIP OR 'O' FOR ONLY:".
       01  LEGEND-REPORT-ONLY.
           03  FILLER           PIC X(38)  VALUE
               "^, v, REPORT ONLY? (Y/N):".
       01  LEGEND-FEE-CODE.
           03  FILLER           PIC X(38)  VALUE
               "^, v, ENTER FEE CODE: ".
       01  LEGEND-CREATE-FILE.
           03  FILLER           PIC X(31)  VALUE
               "^, CREATE EXTRACT FILE? (Y/N): ".
       01  LEGEND-POOL.
           03  FILLER           PIC X(37)  VALUE
               "F7-EXIT, ^, v, ENTER POOL NO, F3-ALL:".
       01  LEGEND-DATE.
           03  FILLER           PIC X(37)  VALUE
               "F7-EXIT, ^, v, ENTER DATE, (MMDDYY):".
