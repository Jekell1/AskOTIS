      * COPYMEMBER: LIBWI/BWCREAW
      *  BAH 170407 CHANGED BWCREA-TRANS-DATE TO 9(8)
      *  KEC 180822 COMMENTED OUT BWCREA-APP-NUMBER
      *  BAH 2024.0415 REMOVED BWCREA-CLMENU-SELECTION,
      *                BWCREA-PASSWD, BWCREA-ERRCD, BWCREA-APP-NUMBER
      *  2024.0503 BAH MADE LEVEL 01, CALL NOT WINDOW
       01  BWCREA-BUF.
           03  BWCREA-WIMENU-SELECTION   PIC XX.
           03  BWCREA-BRANCH           PIC 9(4).
           03  BWCREA-STATE            PIC XX.
           03  BWCREA-DUAL-IN-BRANCH   PIC X.
           03  BWCREA-DUAL-IN-STATE    PIC X.
           03  BWCREA-TRANS-DATE       PIC 9(8).
           03  BWCREA-KEY              PIC X(9).
           03  BWCREA-NO               REDEFINES BWCREA-KEY
                                           PIC 9(9).
           03  BWCREA-SSNO-TBL.
               05  BWCREA-SSNO         OCCURS 4 TIMES
                                           PIC 9(9).
           03  BWCREA-ALLOW-CHG        PIC X.
           03  BWCREA-STATUS           PIC XX.
