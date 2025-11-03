      * COPYMEMBER: LIBLP/EMZ2RG.CMD
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
       01  LEGEND-GTT-01.
           03  FILLER           PIC X(49)  VALUE
               " F7-EXIT, ^, v, ENTER (I)NCLUDE, (S)KIP, (O)NLY :".
       01  LEGEND-CL-01.
           03  FILLER           PIC X(40)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING CLASS NO".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".
       01  LEGEND-CL-02.
           03  FILLER           PIC X(31)  VALUE
               "^, v, ENTER ENDING CLASS NO. : ".
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
       01  LEGEND-OPEN-ACCT.
           03  FILLER           PIC X(49)  VALUE
               "F7-EXIT, F1, ^/v, (I)NCLUDE, (S)KIP, OR (O)NLY ON".
           03  FILLER           PIC X(15)  VALUE
               " OPEN ACCOUNTS:".
       01  LEGEND-REPO-ACCT.
           03  FILLER           PIC X(49)  VALUE
               "F7-EXIT, F1, ^/v, (I)NCLUDE, (S)KIP, OR (O)NLY ON".
           03  FILLER           PIC X(15)  VALUE
               " REPO ACCOUNTS:".
       01  LEGEND-PL-ACCT.
           03  FILLER           PIC X(49)  VALUE
               "F7-EXIT, F1, ^/v, (I)NCLUDE, (S)KIP, OR (O)NLY ON".
           03  FILLER           PIC X(13)  VALUE
               " PL ACCOUNTS:".
       01  LEGEND-PL-TYPE.
           03  FILLER           PIC X(49)  VALUE
               "F7-EXIT, F1, ^/v, (A)LL, (P)L, P(2), P(3), P(4), ".
           03  FILLER           PIC X(8)   VALUE
               "OR P(5):".
       01  LEGEND-PRINT-DETAIL.
           03  FILLER           PIC X(42)  VALUE
               "F7-EXIT, F1, ^/v, PRINT DETAIL ANALYSIS ON".
           03  FILLER           PIC X(16)  VALUE
               " ACCOUNTS (Y/N):".
       01  LEGEND-PRINT-CLASS.
           03  FILLER           PIC X(43)  VALUE
               "F7-EXIT, F1, ^/v, PRINT CLASS TOTALS (Y/N):".
       01  LEGEND-PRINT-BRANCH.
           03  FILLER           PIC X(44)  VALUE
               "F7-EXIT, F1, ^/v, PRINT BRANCH TOTALS (Y/N):".
       01  LEGEND-PRINT-GRAND.
           03  FILLER           PIC X(47)  VALUE
               "F7-EXIT, F1, ^/v, PRINT CORPORATE TOTALS (Y/N):".
