      * COPYMEMBER: LIBLP/AC9DAY.CMD
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/AC1DAY.CMD
      *
      * DESCRIPTION   :  SPECIALIZED WORKERS FOR LP/AC1DAY.C
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 010312 {PR#00000} MERCURY FINANCE [KEVIN MULLEN]
      *            ***NEW*** ADDED BECAUSE SPLIT LP/AC1DAY.C FROM USING
      *            LIBLP/AC1DVR.
      *  CS 170912 ADDED LEGEND-PRINT-LOAN-TYPE-PCX
      ******************************************************************
       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER PRINTER OPTIONS".

      *-----------------------------------------------------------------
       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, v, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH NO. (F3-ALL) ".

      *-----------------------------------------------------------------
       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30)  VALUE "^, v, ENTER ENDING BRANCH NO. ".

      *-----------------------------------------------------------------
       01  LEGEND-BEG-POOL.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING POOL NUMBER      ".

      *-----------------------------------------------------------------
       01  LEGEND-END-POOL.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G POOL NUMBER                 ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCRUAL-DATE.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ^, v, ENTER DATE TO B".
           03  FILLER PIC X(30) VALUE "ASE THE ACCRUAL ON            ".

      *-----------------------------------------------------------------
       01  LEGEND-EXPORT-FILE-YNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE ", (N)O OR (O)NLY TO CREATE EXP".
           03  FILLER PIC X(19) VALUE "ORT FILE           ".

      *-----------------------------------------------------------------
       01  LEGEND-PRINT-BY-CLASS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO REPORT A BREAKDOWN".
           03  FILLER PIC X(19) VALUE " BY CLASS          ".

      *-----------------------------------------------------------------
       01  LEGEND-PRINT-BY-POOL.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO REPORT A BREAKDOWN".
           03  FILLER PIC X(19) VALUE " BY POOL           ".

      *-----------------------------------------------------------------
       01  LEGEND-PRINT-LOAN-TYPE-IB.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO REPORT INTEREST BE".
           03  FILLER PIC X(19) VALUE "ARING LOANS        ".

      *-----------------------------------------------------------------
       01  LEGEND-PRINT-LOAN-TYPE-IBX.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO REPORT INTEREST BE".
           03  FILLER PIC X(19) VALUE "ARING EXCEPTIONS   ".

      *-----------------------------------------------------------------
       01  LEGEND-PRINT-LOAN-TYPE-PC.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO REPORT PRECOMPUTE ".
           03  FILLER PIC X(19) VALUE "LOANS              ".

      *-----------------------------------------------------------------
       01  LEGEND-PRINT-LOAN-TYPE-PCX.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO REPORT PRECOMPUTE ".
           03  FILLER PIC X(19) VALUE "EXCEPTIONS         ".
      *-----------------------------------------------------------------
       01  LEGEND-PRINT-DETAIL-ANALYSIS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO PRINT DETAIL ANALY".
           03  FILLER PIC X(19) VALUE "SIS                ".

      *-----------------------------------------------------------------
       01  LEGEND-PRINT-CLASS-TOTALS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO PRINT TOTALS BY CL".
           03  FILLER PIC X(19) VALUE "ASS                ".

      *-----------------------------------------------------------------
       01  LEGEND-PRINT-POOL-TOTALS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO PRINT TOTALS BY PO".
           03  FILLER PIC X(19) VALUE "OL                 ".

      *-----------------------------------------------------------------
       01  LEGEND-PRINT-BRANCH-TOTALS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO PRINT TOTALS BY BR".
           03  FILLER PIC X(19) VALUE "ANCH               ".

      *-----------------------------------------------------------------
       01  LEGEND-PRINT-GRAND-TOTALS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO PRINT TOTALS BY GR".
           03  FILLER PIC X(19) VALUE "AND                ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-ARROW, (".
           03  FILLER PIC X(30) VALUE "Y)ES OR (N)O TO ACCEPT        ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".
