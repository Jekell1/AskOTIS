      * COPYMEMBER: LIBSP/COLRPT.CMD
      *-----------------------------------------------------------------
       01  LEGEND-ACCTNO-01.
           03  FILLER           PIC X(13) VALUE " F7-EXIT, ^, ".
      * lowercase v
           03  FILLER           PIC X     VALUE X'76'.
           03  FILLER           PIC X(43) VALUE 
               ", ENTER BEGINNING ACCOUNT NUMBER, (F3-ALL):".

       01  LEGEND-ACCTNO-02.
           03  FILLER           PIC X(4)  VALUE " ^, ".
      * lowercase v
           03  FILLER           PIC X     VALUE X'76'.
           03  FILLER           PIC X(30) VALUE 
               ", ENTER ENDING ACCOUNT NUMBER:".
      *-----------------------------------------------------------------
       01  LEGEND-BR-01.
           03  FILLER           PIC X(21)  VALUE
               "F1-PRINTER, F7-EXIT, ".
      * lowercase v
           03  FILLER           PIC X     VALUE X'76'.
           03  FILLER           PIC X(37) VALUE 
               ", ENTER BEGINNING BRANCH NO (F3-ALL):".

       01  LEGEND-BR-01-A.
           03  FILLER           PIC X(51)  VALUE
               "F1-PRINTER, F7-EXIT, v, ENTER BEGINNING BRANCH NO.:".

       01  LEGEND-BR-02.
           03  FILLER           PIC X(31)  VALUE
               "^, v, ENTER ENDING BRANCH NO. :".
      *-----------------------------------------------------------------
       01  LEGEND-CLASS-01.
           03  FILLER           PIC X(42)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING LOAN CLASS".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".

       01  LEGEND-CLASS-02.
           03  FILLER           PIC X(31)  VALUE
               "^, v, ENTER ENDING LOAN CLASS :".
      *-----------------------------------------------------------------
       01  LEGEND-DEALER-01.
           03  FILLER           PIC X(42)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING DEALER NO.".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".

       01  LEGEND-DEALER-02.
           03  FILLER           PIC X(31)  VALUE
               "^, v, ENTER ENDING DEALER NO.:".
      *-----------------------------------------------------------------
       01  LEGEND-COLL-01.
           03  FILLER           PIC X(44)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING COLLECTOR ID".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".

       01  LEGEND-COLL-02.
           03  FILLER           PIC X(33)  VALUE
               "^, v, ENTER ENDING COLLECTOR ID :".
      *-----------------------------------------------------------------
       01  LEGEND-NAME-01.
           03  FILLER           PIC X(41)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING LAST NAME".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".

       01  LEGEND-NAME-02.
           03  FILLER           PIC X(30)  VALUE
               "^, v, ENTER ENDING LAST NAME :".

      *-----------------------------------------------------------------
       01  LEGEND-COLLID-01.
           03  FILLER           PIC X(44)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING COLLECTOR ID".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".

       01  LEGEND-COLLID-02.
           03  FILLER           PIC X(33)  VALUE
               "^, v, ENTER ENDING COLLECTOR ID :".
      *-----------------------------------------------------------------
       01  LEGEND-PROMISED-01.
           03  FILLER           PIC X(50)  VALUE
              "F7-EXIT, ^, v, (I)NCLUDE, (S)KIP, (O)NLY PROMISED ".
           03  FILLER           PIC X(9)  VALUE
               "ACCOUNTS:".
       01  LEGEND-PROMDATE-01.
           03  FILLER           PIC X(45)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING PROMISED DATE".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".

       01  LEGEND-PROMDATE-02.
           03  FILLER           PIC X(34)  VALUE
               "^, v, ENTER ENDING PROMISED DATE :".
      *-----------------------------------------------------------------
       01  LEGEND-CONTRACTUAL-01.
           03  FILLER           PIC X(47)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING CONTRACTUAL AGE".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".

       01  LEGEND-CONTRACTUAL-02.
           03  FILLER           PIC X(36)  VALUE
               "^, v, ENTER ENDING CONTRACTUAL AGE :".
      *-----------------------------------------------------------------
       01  LEGEND-DATE-01.
           03  FILLER           PIC X(37)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING DATE:".

       01  LEGEND-DATE-02.
           03  FILLER           PIC X(30)  VALUE
               "^, v, ENTER ENDING DATE : ".
      *-----------------------------------------------------------------
       01  LEGEND-TIME-01.
           03  FILLER           PIC X(37)  VALUE
               " F7-Exit, ^, v, Enter beginning time:".

       01  LEGEND-TIME-02.
           03  FILLER           PIC X(37)  VALUE
               " F7-Exit, ^, v, Enter ending time:   ".

       01  LEGEND-PL-01.
           03  FILLER           PIC X(48)  VALUE
              "F7-EXIT, ^, v, (I)NCLUDE, (S)KIP, (O)NLY FOR P&L".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".
       01  LEGEND-PL-02.
           03  FILLER           PIC X(43)  VALUE
              "F7-EXIT, ^, v, (A)CTIVE, (I)NACTIVE, (B)OTH".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".

      *-----------------------------------------------------------------
       01  LEGEND-PASTDUE-01.
           03  FILLER           PIC X(45)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING PAST DUE DAYS".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".

       01  LEGEND-PASTDUE-02.
           03  FILLER           PIC X(34)  VALUE
               "^, v, ENTER ENDING PAST DUE DAYS :".
      *-----------------------------------------------------------------
       01  LEGEND-PRINT-DETAIL.
           03  FILLER           PIC X(51)  VALUE
               "^, v, ENTER (Y)ES OR (N)O TO PRINT DETAIL ANALYSIS:".

       01  LEGEND-PRINT-MEMO.
           03  FILLER           PIC X(41)  VALUE
               "^, v, ENTER (Y)ES OR (N)O TO PRINT MEMOS:".

       01  LEGEND-PRINT-COLLID-TOTAL.
           03  FILLER           PIC X(52)  VALUE
               "^, v, ENTER (Y)ES OR (N)O TO PRINT COLLECTOR TOTALS:".

       01  LEGEND-PRINT-BRANCH-TOTAL.
           03  FILLER           PIC X(49)  VALUE
               "^, v, ENTER (Y)ES OR (N)O TO PRINT BRANCH TOTALS:".

       01  LEGEND-PRINT-CORP-TOTAL.
           03  FILLER           PIC X(52)  VALUE
               "^, v, ENTER (Y)ES OR (N)O TO PRINT CORPORATE TOTALS:".

       01  LEGEND-PRINT-CON-OR-REC.
           03  FILLER           PIC X(51)  VALUE
               "^, v, ENTER C OR R TO PRINT CONTRACUTAL OR RECENCY:".

       01  LEGEND-PRINT-RECENCY-TOTAL.
           03  FILLER           PIC X(50)  VALUE
               "^, v, ENTER Y OR N TO PRINT RECENCY RESULT TOTALS:".

       01  LEGEND-NO-PRINTING.
           03  FILLER           PIC X(39)  VALUE
               "THIS SETUP DOES NOT PRINT ANY REPORT!!!".
      *-----------------------------------------------------------------
       01  LEGEND-POT30-01.
           03  FILLER           PIC X(52)  VALUE
               "^, v, (I)NCLUDE, (S)KIP, (O)NLY FOR POTENTIAL 30's :".

       01  LEGEND-CUTOFF-DATE-01.
           03  FILLER           PIC X(48)  VALUE
               "^, v, LIST NON-WORKED ACCOUNTS AFTER THIS DATE :".

       01  LEGEND-USE-RECODE.
           03  FILLER           PIC X(48)  VALUE
               "^, v, USE RESULT CODE WORKED FLAG (Y/N) :".

       01  LEGEND-ALPHA-PRINT-SEQ.
           03  FILLER           PIC X(48)  VALUE
               "^, v, PRINT IN BORROWER NAME ORDER(Y/N) :".

       01  LEGEND-PRINT-SEQ.
           03  FILLER           PIC X(53)  VALUE
               "ORDER: A=ALPHA,B=DESCENDING BAL,C=COLLECTOR,N=NORMAL:".


       01  LEGEND-RESULT-CODE.
           03  FILLER           PIC X(33)  VALUE
               "^, v, F6-SCAN, ENTER RESULT CODE:".

       01  LEGEND-NO-CORP-TOTAL.
           03  FILLER           PIC X(49)  VALUE
               "CORPORATE TOTALS NOT ALLOWED ON LOCAL-SPOOLING!!!".

       01  LEGEND-PRINT-REPORT.
           03  FILLER           PIC X(51)  VALUE
               "^, v, ENTER (Y)ES OR (N)O TO PRINT DETAIL REPORT:".

       01  LEGEND-PRINT-EXTRA-DETAIL.
           03  FILLER           PIC X(52)  VALUE
               "^, v, ENTER Y OR N TO PRINT TROUBLE-SHOOTING DETAIL:".


       01  LEGEND-LOCAL-SPOOL.
           03  filler           pic x(31)  value
               "SPOOL TO LOCAL DIRECTORY (Y/N):".

       01  LEGEND-ACCEPT.
           03  FILLER           PIC X(40)  VALUE
               "F7-EXIT, F1, ^, ACCEPT (Y/N):".

       01  BABY-ZS              PIC X(3)   VALUE "zzz".
