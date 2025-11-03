      * COPYMEMBER: LIBWI/TRJSCNW
      ******************************************************************
      *
      * DESCRIPTION: TRANSACTION FILE "S" SALES FINANCE SCAN WORKERS
      *
      * REV:
      *  2024.0501 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  TRJSCN-BUF.
           03  TRJSCN-TRJ1-KEY       PIC X(28).
           03  TRJSCN-BRNO           PIC 9(4).
           03  TRJSCNW-MMYY          PIC 9999.
           03  TRJSCNW-MMYYX REDEFINES TRJSCNW-MMYY.
               05  TRJSCNW-MM        PIC 99.
               05  TRJSCNW-YY        PIC 99.
           03  TRJSCN-RETURN         PIC X.
           03  TRJSCN-STATUS         PIC XX.
