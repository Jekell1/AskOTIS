      * COPYMEMBER: LIBEX/EX01XCA
      **************************************************************************
      * CASH DRAWER DATA WAREHOUSE EXTRACTION
      * 
      * REV:
      *  2021.0804 BAH  NEW #1509
      **************************************************************************
       78  XCA-SIZE                        VALUE 142.
       01  XCA-REC.
           03  XCA-BRNO                 PIC 9(4).
           03  XCA-DATE                 PIC 9(8).
           03  XCA-TILLNO-ZERO          PIC 9.
           03  XCA-TILLNO               PIC X.
           03  XCA-DESC                 PIC X(30).
           03  XCA-BEGBAL               PIC +9(7).99.
           03  XCA-OPENED               PIC 9(4).
           03  XCA-CLOSED               PIC 9(4).
           03  XCA-ENDBAL               PIC +9(7).99.
           03  XCA-BALBY                PIC X(10).
           03  XCA-RUNNING              PIC +9(7).99.
           03  XCA-DEPOSIT              PIC +9(7).99.
           03  XCA-OPENID               PIC X(8).
           03  XCA-NET-CASH-NOTES       PIC +9(7).99.
           03  XCA-PASSWORD             PIC X(4).
           03  XCA-CHECK-DEPOSIT        PIC +9(7).99.
           03  XCA-CR                   PIC X(01).
           03  XCA-LF                   PIC X(01).

