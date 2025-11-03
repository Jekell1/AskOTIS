      * COPYMEMBER: LIBSP/CJPURG.CMD
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
       01  LEGEND-COLL-01.
           03  FILLER           PIC X(44)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING COLLECTOR ID".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".
       01  LEGEND-COLL-02.
           03  FILLER           PIC X(33)  VALUE
               "^, v, ENTER ENDING COLLECTOR ID :".
       01  LEGEND-DATE-01.
           03  FILLER           PIC X(36)  VALUE
               " F7-EXIT, ^, v, ENTER BEGINNING DATE".
           03  FILLER           PIC X(11)  VALUE
               ". (F3-ALL):".
       01  LEGEND-DATE-02.
           03  FILLER           PIC X(30)  VALUE
               "^, v, ENTER ENDING DATE : ".
       01  LEGEND-PRINT-MEMO.
           03  FILLER           PIC X(41)  VALUE
               "^, v, ENTER (Y)ES OR (N)O TO PRINT MEMOS:".
       01  LEGEND-PURGE-MEMO.
           03  FILLER           PIC X(46)  VALUE
               "^, v, ENTER (Y)ES OR (N)O TO PURGE LOAN MEMOS:".
       01  LEGEND-LOCAL-SPOOL.
           03  filler           pic x(31)  value
               "SPOOL TO LOCAL DIRECTORY (Y/N):".
       01  LEGEND-ACCEPT.
           03  FILLER           PIC X(40)  VALUE
               "F7-EXIT, F1, ^, ACCEPT (Y/N):".
       01  BABY-ZS              PIC X(3)   VALUE "zzz".
