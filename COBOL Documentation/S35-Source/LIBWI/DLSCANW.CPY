      * COPYMEMBER: LIBWI/DLSCANW
      *  2024.0502 BAH MADE LEVEL 01, CALL NOT WINDOW
       01  DLSCAN-BUF.
           07  DLSCAN-NUM        PIC 9(6).
           07  DLSCAN-DLNO       REDEFINES DLSCAN-NUM
                                 PIC 9(4)V99.
           07  DLSCAN-NAME       PIC X(30).
           07  DLSCAN-STATUS     PIC XX.
