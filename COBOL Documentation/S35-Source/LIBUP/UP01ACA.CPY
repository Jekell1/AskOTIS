      * COPYMEMBER: LIBUP/UP01ACA
      **************************************************
      *             ( ASCII CASH DRAWER FILE )
      *
      *  TAB DELMITED
      * 210423 REMOVED FUTURE FIELDS
      **************************************************
       78  ACA-SIZE                     VALUE 166.
       01  ACA-REC.
           03  ACA-BRNO                 PIC 9(4).
           03  FILLER                   PIC X.
           03  ACA-DATE                 PIC 9(8).
           03  FILLER                   PIC X.
           03  ACA-TILLNO               PIC 99.
           03  FILLER                   PIC X.
           03  ACA-LTOUCH-DATE          PIC 9(8).
           03  FILLER                   PIC X.
           03  ACA-DESC                 PIC X(30).
           03  FILLER                   PIC X.
           03  ACA-BEGBAL               PIC +9(7).99.
           03  FILLER                   PIC X.
           03  ACA-OPENED-HHMM          PIC 9(4).
           03  FILLER                   PIC X.
           03  ACA-CLOSED-HHMM          PIC 9(4).
           03  FILLER                   PIC X.
           03  ACA-ENDBAL               PIC +9(7).99.
           03  FILLER                   PIC X.
           03  ACA-BALBY                PIC X(10).
           03  FILLER                   PIC X.
           03  ACA-RUNNING              PIC +9(7).99.
           03  FILLER                   PIC X.
           03  ACA-DEPOSIT              PIC +9(7).99.
           03  FILLER                   PIC X.
           03  ACA-OPEN-USERID          PIC X(8).
           03  FILLER                   PIC X.
           03  ACA-NET-CASH-NOTES       PIC +9(7).99.
           03  FILLER                   PIC X.
           03  ACA-PASSWORD             PIC X(4).
           03  FILLER                   PIC X.
           03  ACA-CHECK-DEPOSIT        PIC +9(7).99.
           03  FILLER                   PIC X.

           03  ACA-CR                   PIC X.
           03  ACA-LF                   PIC X.
