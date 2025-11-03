      ***********************************************************
      *   ASCII LOAN MEMO FILE         
      *
      *  TAB DELMITED
      *
      ***********************************************************
       78  ALM-SIZE                       VALUE 118.
       01  ALM-REC.
           03  ALM-BRNO               PIC 9(4).
           03  FILLER                 PIC X.
           03  ALM-ACCTNO             PIC 9(8).
           03  FILLER                 PIC X.
           03  ALM-DATE-REVERSED      PIC 9(8).
           03  FILLER                 PIC X.
           03  ALM-SEQ                PIC 9(3).
           03  FILLER                 PIC X.
           03  ALM-LTOUCH-DATE        PIC 9(8).
           03  FILLER                 PIC X.
           03  ALM-LTOUCH-TIME-HHMM   PIC 9(4).
           03  FILLER                 PIC X.
           03  ALM-LTOUCH-USERID      PIC X(10).
           03  FILLER                 PIC X.
           03  ALM-MEMO               PIC X(60).
           03  FILLER                 PIC X.
           03  ALM-MEMO-TYPE          PIC XX.
           03  FILLER                 PIC X.

           03  ALM-CR                 PIC X.
           03  ALM-LF                 PIC X.
