      * COPYMEMBER: LIBWI/CJSCANW
      * MJD 131209 ADJSTED VALUES FOR A30.
      * BAH 2024.0415 REMOVED CJSCANW-NOSTOP-FG, CJSCANW-SCAN-ONLY-FG
      * BAH 2024.0424 MADE 01 LEVEL, CALL INSTEAD OF WINDOW
      *               REMOVED CJSCANW-TYPE
       01  CJSCAN-BUF.
           03  CJSCANW-CJ1-KEY.
               05  CJSCANW-BRNO            PIC 9(4).
               05  CJSCANW-COLLID          PIC X(3).
               05  CJSCANW-DATE            PIC 9(8).
               05  CJSCANW-START-TIME      PIC 9(4).
               05  CJSCANW-SEQNO           PIC 9(2).
           03  CJSCANW-STATUS              PIC XX.
