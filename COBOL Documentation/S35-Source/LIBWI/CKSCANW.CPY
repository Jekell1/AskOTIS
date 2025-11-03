      * COPYMEMBER: LIBWI/CKSCANW
      * REV:
      *  20240416 BAH REMOVED CKSCAN-SCAN-ONLY-FG, CKSCAN-CK2-KEY,
      *               CKSCAN-PAYEE
      *  20240424 BAH MADE LEVEL 01, CALL NOT A WINDOW
       01  CKSCAN-BUF.
           03  CKSCAN-CK1-KEY.
               05  CKSCANW-BRNO   PIC 9(4).
               05  CKSCANW-NO     PIC 9(6).
      *-------------------------------------------------------
      * CKSCAN-NOSTOP-FG:
      *    " " - YES, STOP FOR INITIAL SCANKEY ENTRY
      *    "Y" - NO , STOP FOR INITIAL SCANKEY ENTRY
      *-------------------------------------------------------
           03  CKSCAN-NOSTOP-FG     PIC X    VALUE " ".
           03  CKSCAN-STATUS        PIC XX.
