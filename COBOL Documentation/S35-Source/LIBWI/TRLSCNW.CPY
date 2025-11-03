      * COPYMEMBER: LIBWI/TRLSCNW
      ******************************************************************
      *
      * DESCRIPTION: TRANSACTION FILE "L" LOAN SCAN WORKERS
      *
      * REV:
      *  2024.0501 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  TRLSCN-BUF.
           03  TRLSCN-TRL1-KEY       PIC X(28).
           03  TRLSCN-BRNO           PIC 9(4).
           03  TRLSCNW-MMYY          PIC 9999.
           03  TRLSCNW-MMYYX REDEFINES TRLSCNW-MMYY.
               05  TRLSCNW-MM        PIC 99.
               05  TRLSCNW-YY        PIC 99.
           03  TRLSCN-RETURN         PIC X.
           03  TRLSCN-STATUS         PIC XX.
