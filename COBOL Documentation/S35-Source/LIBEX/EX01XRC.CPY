      *****************************************************************
      *  DAILY STATUS RECAP FILE
      *
      *  DATA WAREHOUSE EXTRACTION
      *
      * REV:
      *  BAH 2021026 NEW #1575
      *****************************************************************
       78  XRC-SIZE                                VALUE 256.
       01  XRC-REC.
           03  XRC-STATUS                  PIC XX.
           03  XRC-BRNO                    PIC 9(4).
           03  XRC-TRANS-DATE              PIC 9(8).
           03  XRC-LTOUCH-DATE             PIC 9(8).
           03  XRC-RECAP-FG                PIC X.
           03  XRC-GL-FG                   PIC X.
           03  XRC-EOD-BATCH-FG            PIC X.
           03  XRC-NIGHTLY-FUNDS-FG        PIC X.
           03  XRC-EOM1-BATCH-FG           PIC X.
           03  XRC-EOM-UPDATE-FG           PIC X.
           03  XRC-EOM2-BATCH-FG           PIC X.
           03  XRC-BS-P2P3-INBAL-FG        PIC X.
           03  XRC-DEPOSIT                 PIC +9(7).99.
           03  XRC-OVER-SHORT              PIC +9(7).99.
           03  XRC-CHECKS                  PIC +9(7).99.
           03  XRC-OPENED-DATE             PIC 9(8).
           03  XRC-OPENED-TIME             PIC 9(6).
           03  XRC-OP-USERID               PIC X(10).
           03  XRC-CLOSED-DATE             PIC 9(8).
           03  XRC-CLOSED-TIME             PIC 9(6).
           03  XRC-CL-USERID               PIC X(10).
           03  XRC-UPDATED-DATE            PIC 9(8).
           03  XRC-UPDATED-TIME            PIC 9(6).
           03  XRC-UPDATED-USERID          PIC X(10).
           03  XRC-EOD-BATCH-DATE          PIC 9(8).
           03  XRC-EOD-BATCH-TIME          PIC 9(6).
           03  XRC-FUNDS-DATE              PIC 9(8).
           03  XRC-FUNDS-TIME              PIC 9(6).
           03  XRC-EARNINGS-DATE           PIC 9(8).
           03  XRC-EARNINGS-TIME           PIC 9(6).
           03  XRC-EOM-UPDATED-DATE        PIC 9(8).
           03  XRC-EOM-UPDATED-TIME        PIC 9(6).
           03  XRC-EOM2-BATCH-DATE         PIC 9(8).
           03  XRC-EOM2-BATCH-TIME         PIC 9(6).
           03  XRC-CHECK-DATE              PIC 9(8).
           03  XRC-CHECK-TIME              PIC 9(6).
           03  XRC-LAST-ACCRUE             PIC 9(4).
           03  XRC-ERROR-MSG               PIC X(15).
           03  XRC-BANK-BAL                PIC S9(9).
           03  XRC-CHECK-DEPOSIT           PIC S9(7).
           03  XRC-CR                      PIC X(01).
           03  XRC-LF                      PIC X(01).
