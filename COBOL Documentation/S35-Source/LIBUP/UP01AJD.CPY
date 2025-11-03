      ******************************************************************
      * ASCII GENERAL LEDGER JOURNAL DETAIL
      *  TAB DELMITED
      *
      ******************************************************************
       78  AJD-SIZE                             VALUE 126.
       01  AJD-REC.
           03  AJD-DATE                     PIC 9(8).
           03  FILLER                       PIC X.
           03  AJD-ID                       PIC X(8).
           03  FILLER                       PIC X.
           03  AJD-SEQ                      PIC 9(2).
           03  FILLER                       PIC X.
           03  AJD-LINE                     PIC 9(4).
           03  FILLER                       PIC X.
           03  AJD-LTOUCH-DATE              PIC 9(08).
           03  FILLER                       PIC X.
           03  AJD-GROUP-FG                 PIC X.
           03  FILLER                       PIC X.
           03  AJD-BR-ACCOUNT               PIC 9(11).
           03  FILLER                       PIC X.
           03  AJD-OFFSET-GROUP-FG          PIC X.
           03  FILLER                       PIC X.
           03  AJD-OFFSET-BR-ACCOUNT        PIC 9(11).
           03  FILLER                       PIC X.
           03  AJD-AMT                      PIC +9(9).99.
           03  FILLER                       PIC X.
           03  AJD-EXPL                     PIC X(32).
           03  FILLER                       PIC X.
           03  AJD-DRCR-FG                  PIC X.
           03  FILLER                       PIC X.
           03  AJD-STATUS                   PIC X.
           03  FILLER                       PIC X.
           03  AJD-ORIG-LINE                PIC 9(4).
           03  FILLER                       PIC X.
           03  AJD-PARENT-GROUP             PIC X(4).
           03  FILLER                       PIC X.

           03  AJD-CR                       PIC X.
           03  AJD-LF                       PIC X.
