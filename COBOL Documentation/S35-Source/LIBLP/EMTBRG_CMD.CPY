      * COPYMEMBER: LIBLP/EMTBRG.CMD
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
       01  LEGEND-GTT-01.
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
       01  legend-printcol-01.
           03  filler           pic x(47)  value
               "PRINT COLLATERAL INFORMATION WITH REPORT (Y/N):".
       01  LEGEND-LOCAL-SPOOL.
           03  filler           pic x(31)  value
               "SPOOL TO LOCAL DIRECTORY (Y/N):".
       01  LEGEND-ACCEPT.
           03  FILLER           PIC X(40)  VALUE
               "F7-EXIT, F1, ^, ACCEPT (Y/N):".
