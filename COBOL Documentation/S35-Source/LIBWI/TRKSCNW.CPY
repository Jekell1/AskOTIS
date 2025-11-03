      * COPYMEMBER: LIBWI/TRKSCNW
      ******************************************************************
      *
      * DESCRIPTION: TRANSACTION FILE "S" SALES FINANCE SCAN WORKERS
      *
      * REV:
      *  2024.0501 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  TRKSCN-BUF.
           03  TRKSCN-TRK1-KEY       PIC X(28).
           03  TRKSCN-BRNO           PIC 9(4).
           03  TRKSCNW-MMYY          PIC 9999.
           03  TRKSCNW-MMYYX REDEFINES TRKSCNW-MMYY.
               05  TRKSCNW-MM        PIC 99.
               05  TRKSCNW-YY        PIC 99.
           03  TRKSCN-RETURN         PIC X.
           03  TRKSCN-STATUS         PIC XX.
