      * COPYMEMBER: LIBWI/LNSCANW
      * REV:
      *  2024.0417 BAH CHANGED LNSCAN-NO TO 8 FROM 6
      *  2024.0426 BAH MADE LEVEL 01 AND CALL, NOT WINDOW
       01  LNSCAN-BUF.
           03  LNSCAN-NO         PIC 9(8).
           03  LNSCAN-SSNO-TBL.
               05  LNSCAN-SSNO   OCCURS 4 TIMES
                                 PIC 9(9).
           03  LNSCAN-SYSDATE    PIC X(8).
           03  LNSCAN-STATUS     PIC XX.
