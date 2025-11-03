      * COPYMEMBER: LIBWI/TZSCANW
      ************************************************************
      * REV:
      *  2024.0506 BAH MADE LEVEL 01, CALL NOT WINDOW
      ************************************************************
       01  TZSCAN-BUF.
           03  TZSCAN-TZ            PIC 9(6).
           03  TZSCAN-KEY REDEFINES TZSCAN-TZ.
               05  TZSCANW-AC        PIC 9(03).
               05  TZSCANW-PF        PIC 9(03).
           03  TZSCAN-PF-ONLY       PIC X(01).
           03  TZSCAN-PF-AUTOSTART  PIC X(01).
           03  TZSCAN-STATUS        PIC XX.

