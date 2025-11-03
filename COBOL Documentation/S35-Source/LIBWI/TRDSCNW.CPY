      * COPYMEMBER: LIBWI/TRDSCNW
      ******************************************************************
      *
      * DESCRIPTION: TRANSACTION FILE "C" CASH/CHECK SCAN WORKERS
      *
      * REV:
      *  2024.0501 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  TRDSCN-BUF.
           03  TRDSCN-TRD1-KEY       PIC X(12).
           03  TRDSCN-BRNO           PIC 9(4).
           03  TRDSCNW-MMYY          PIC 9999.
           03  TRDSCNW-MMYYX REDEFINES TRDSCNW-MMYY.
               05  TRDSCNW-MM        PIC 99.
               05  TRDSCNW-YY        PIC 99.
           03  TRDSCN-RETURN         PIC X.
           03  TRDSCN-STATUS         PIC XX.
