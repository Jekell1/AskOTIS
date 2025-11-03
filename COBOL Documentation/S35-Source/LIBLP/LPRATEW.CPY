      * COPYMEMBER: LIBLP/LPRATEW
      **************************************************************************
      *    SET RATE FOR IB CALC WORKERS
      * REV:
      *  SLC 890125 ADDED LPINDUW.
      *  SLC 891005 ADDED INDU-MAX-ACCRUE FOR I/B ACCRUE REPORT
      *  JTG 000107 INDU-LIST & INDU-COMM RE: SP-RBREDUC = 9999.04
      *             WISCONSIN IB REAL ESTATE PREPAY PENALTY, WISCONSIN #1031
      *  JTG 020722 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *   CS 100609 ADDED INDU-ASSESS-PENALTY-16. IF SP-RBSOPT1 16, AND ACCOUNT
      *             HAD INTEREST TO EXPIRDATE & THEN EXPIRED INTEREST, THE
      *             INTEREST-DUE-CALCULATION CALCULATED A PENALTY TWICE
      *                                                     REGENCY PL#689
      *  BAH 161025 EXPANDED ALL DATES TO 9(8)
      *  BAH 2024.0412 REMOVED RATE-INT-DATE, INDU-SUB, INDU-LIST
      **************************************************************************
       01  RATE-WORKERS.
           03  RATE-DATE       PIC 9(8)     COMP  VALUE 0.
           03  RATE-MAKERCD    PIC X              VALUE " ".
           03  RATE-EXP-INT    PIC S9(5)V99 COMP.
           03  RATE-LPTRCD     PIC XX VALUE "  ".
           03  RATE-EXPR-TAB.
               05  RATE-EXPR-TAB-BREAK PIC 9(8)
                                       OCCURS 5 TIMES.
           03  RATE-EXPR-BREAK PIC 9(6)V99.
           03  RATE-MONTH-DAY-BREAK REDEFINES RATE-EXPR-BREAK.
               05  RATE-EXPR-MONTHS    PIC 9(6).
               05  RATE-EXPR-DAYS      PIC 99.
      *-------------------------------------------------------------------------
      *   INTEREST-DUE WORKERS
      * REV:
      *  JTG 880331 ADDED INDU-LPTRCD FOR SPECIAL REFUND
      *             CONDITIONS
      *  JTG 900125 ADDED INDU-DAYS AS OUTPUT FIELD
      *  JTG 910309 ADDED INDU-WK FOR DATER
      *  JTG 000105 ADDED SP-RBREDUC = 9999.04 WISCONSIN IB REAL ESTATE
      *             PREPAY PENALTY, WISCONSIN #1031
      *             INDU-LIST, INDU-BUF AND INDU-PENALTY
      *  JTG 020722 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *-------------------------------------------------------------------------
       01  INDU-INTEREST        PIC S9(5)V99   COMP VALUE 0.
       01  INDU-DAYS            PIC S9(5)      COMP VALUE 0.
       01  INDU-WORKER          PIC S9(6)V9(7) COMP VALUE 0.
       01  INDU-CURBAL          PIC S9(7)V99   COMP VALUE 0.
       01  INDU-RATE            PIC 999V999    COMP VALUE 0.
       01  INDU-AMOUNT          PIC S9(7)V99   COMP VALUE 0.
       01  INDU-ALL-INSPREM     PIC S9(7)V99   COMP VALUE 0.
       01  INDU-DATE-1          PIC 9(8)       COMP VALUE 0.
       01  INDU-DATE-2          PIC 9(8)       COMP VALUE 0.
       01  INDU-LPTRCD          PIC XX              VALUE " ".
       01  INDU-YEARTYPE        PIC 9(4)       COMP VALUE 0.
       01  INDU-INTMETHOD       PIC X               VALUE " ".
       01  INDU-INTEREST-2      PIC S9(5)V99   COMP VALUE 0.
       01  INDU-MAX-ACCRUE      PIC 9          COMP VALUE 0.
       01  INDU-WK              PIC 999        COMP VALUE 0.
       01  INDU-ASSESS-PENALTY  PIC X            VALUE " ".
       01  HOLD-INDU-PENALTY    PIC S9(5)V99    COMP VALUE 0.
       01  INDU-BUF.
           03  INDU-PENALTY     PIC S9(5)V99   VALUE 0.

       01  INDU-COMMON.
           03  INDU-COMMON-RTNCD       PIC X.
           03  INDU-COMMON-REBATE      PIC S9(7)V99 COMP-3.
           03  INDU-COMMON-AREA        PIC X(40).
      * USED FOR WISCONSIN REDUCTION CODE:
           03  INDU-A02               REDEFINES INDU-COMMON-AREA.
               05  INDU-A02-PENALTY    PIC S9(7)V99 COMP-3.
               05  INDU-A02-PAYDATE    PIC 9(8)     COMP-3.
               05  FILLER              PIC X(30).
      * USED FOR MISSISSIPPI REDUCTION CODE:
           03  INDU-A01               REDEFINES INDU-COMMON-AREA.
               05  INDU-A01-ELAPSED-MO PIC S9(3)    COMP-3.
               05  INDU-A01-PENALTY    PIC S9(5)V99 COMP-3.
               05  INDU-A01-PAYDATE    PIC 9(8)     COMP-3.
               05  FILLER              PIC X(29).
