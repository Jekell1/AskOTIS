      * COPYMEMBER: LIBLP/AFPDLLPW
      ******************************************************************
      *
      *                      (AFPDLLPW)
      *
      *    WORKERS FOR ALTERNATE FINANCE PLAN (AFP) DUE DEALER FOR
      *    THE LP-FILE.
      *
      ******************************************************************
       01  AFPDLLP-TODAY            PIC 9(8)          COMP   VALUE 0.
       01  AFPDLLP-APCUR-PERIOD     PIC S9(6)V99      COMP   VALUE 0.
       01  AFPDLLP-APCUR-TD         PIC  9(6)V99      COMP   VALUE 0.
       01  AFPDLLP-DELINQ-PYMTS     PIC S9(6)V99      COMP   VALUE 0.
       01  AFPDLLP-DUE-DEALER       PIC S9(6)V99      COMP   VALUE 0.
       01  AFPDLLP-PER              PIC  9(4)V9(12)   COMP   VALUE 0.
       01  AFPDLLP-PRIOR-PAYABLE    PIC S9(6)V99      COMP   VALUE 0.
       01  AFPDLLP-REMAINING-ADV    PIC S9(6)V99      COMP   VALUE 0.
       01  AFPDLLP-REMAINING-ADV-RV PIC S9(6)V99      COMP   VALUE 0.
       01  AFPDLLP-THIS-PAYABLE     PIC S9(6)V99      COMP   VALUE 0.
       01  AFPDLLP-THIS-PAYABLE-RV  PIC S9(6)V99      COMP   VALUE 0.
       01  AFPDLLP-TOT-PAYABLE      PIC S9(6)V99      COMP   VALUE 0.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    AFPDLLP-PROCESSED IS TO DETERMINED IF AFPDLLP CALCULATIONS
      *    WERE OR WERE NOT PROCESS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  AFPDLLP-PROCESS          PIC X(1)                 VALUE " ".
