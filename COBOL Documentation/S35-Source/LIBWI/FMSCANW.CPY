      * COPYMEMBER: LIBWI/FMSCANW
      * REV:
      *  2024.0416 BAH REMOVED FMSCAN-FM-PRINT, CHANGED LEVEL TO 01, CALL 
       01  FMSCAN-BUF.
           03  FMSCAN-KEY            PIC 9(4).
           03  FMSCAN-VERIFY.
               05  FMSCAN-FM-INITKEY PIC X(20).
               05  FMSCAN-FM-FMNO    PIC 9(4).
           03  FMSCAN-STATUS         PIC XX.
