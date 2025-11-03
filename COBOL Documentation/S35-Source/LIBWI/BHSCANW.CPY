      * COPYMEMBER: LIBWI/BHSCANW
      ******************************************************************
      *
      * REV:
      *  2024.0506 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  BHSCAN-BUF.
           03  BHSCAN-BH-KEY.
               05  BHSCAN-USERID        PIC X(10).
               05  BHSCAN-DATE          PIC 9(8).
               05  BHSCAN-TIME          PIC 9(4) COMP-3.
           03  BHSCAN-STATUS            PIC XX.
