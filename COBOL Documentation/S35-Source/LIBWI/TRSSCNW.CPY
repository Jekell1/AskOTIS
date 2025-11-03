      * COPYMEMBER: LIBWI/TRSSCNW
      ******************************************************************
      *
      * DESCRIPTION: TRANSACTION FILE "S" SALES FINANCE SCAN WORKERS
      *
      * REV:
      *  2024.0501 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  TRSSCN-BUF.
           03  TRSSCN-TRS1-KEY       PIC X(28).
           03  TRSSCN-BRNO           PIC 9(4).
           03  TRSSCNW-MMYY          PIC 9999.
           03  TRSSCNW-MMYYX REDEFINES TRSSCNW-MMYY.
               05  TRSSCNW-MM        PIC 99.
               05  TRSSCNW-YY        PIC 99.
           03  TRSSCN-RETURN         PIC X.
           03  TRSSCN-STATUS         PIC XX.
