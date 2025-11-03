       01 REBATE-LINK.
      * INPUT FIELDS:
           03  REB-PAYDATE            PIC 9(8)             VALUE 0.
           03  REB-ORGTERM            PIC S9(5)            VALUE 0.
           03  REB-SUB                PIC 99               VALUE 0.
           03  REB-SUB2               PIC 99               VALUE 0.
               88  PODATE-B4-1STPYDATE                     VALUE 1.
           03  REB-TOTCHG             PIC S9(6)V99         VALUE 0.
           03  REB-LPTRCD             PIC XX               VALUE " ".
           03  REB-SKIPDEF-FG         PIC X                VALUE " ".
           03  REB-ADDON-FG           PIC X                VALUE " ".
           03  REB-RECAST             PIC X                VALUE " ".
           03  REB-DEF-STOP           PIC X                VALUE " ".
           03  REB-EARN-FULL-MONTH    PIC X                VALUE " ".
           03  REB-STATEWK            PIC S9(7)V99.

      * INPUT FIELDS FOR ADDON INSURANCE:
           03  REB-LN-PURDATE         PIC 9(8)             VALUE 0.
           03  REB-LN-LOANDATE        PIC 9(8)             VALUE 0.
           03  REB-LN-INTDATE         PIC 9(8)             VALUE 0.
           03  REB-LN-ORIG-1STPYDATE     PIC 9(8)             VALUE 0.
           03  REB-LN-TOTNODEF        PIC S9(3)            VALUE 0.
           03  REB-LN-INSCOVR         PIC 9(6)V99          VALUE 0.
           03  REB-LN-SALEFIN         PIC X                VALUE " ".
           03  REB-LN-INSEFF          PIC 9(8)             VALUE 0.
           03  REB-LN-INSEXP          PIC 9(8)             VALUE 0.
           03  REB-LN-APRATE          PIC 9(3)V9(3)        VALUE 0.
           03  REB-LN-SMPRATE         PIC 9(3)V9(3)        VALUE 0.
           03  REB-LN-ORGTERM         PIC 9(3)             VALUE 0.
           03  REB-LN-1STPYAMT        PIC 9(6)V99          VALUE 0.
           03  REB-LN-REGPYAMT        PIC 9(6)V99          VALUE 0.
           03  REB-LN-LASTPYAMT       PIC 9(6)V99          VALUE 0.
           03  REB-LN-INT-REBATE      PIC 9(6)V99          VALUE 0.
           03  REB-LN-SC-REBATE       PIC 9(6)V99          VALUE 0.
           03  REB-LN-MF-REBATE       PIC 9(6)V99          VALUE 0.
           03  REB-LN-DF-REBATE       PIC 9(6)V99          VALUE 0.

      * OUTPUT WORKERS:
           03  REB-REBATE             PIC S9(7)V99.
           03  FILLER                REDEFINES REB-REBATE.
               05  REB-REBATE-WHOLE   PIC S9(7).
               05  REB-REBATE-CENTS   PIC 99.
           03  REB-PRES-VALUE         PIC S9(7)V99.
           03  REB-REMTERM            PIC S9(5).
           03  REB-MS-PENALTY         PIC S9(5)V99.
           03  INS-TAX-RB             PIC S9(7)V99.
           03  REB-EP54-AMT           PIC S9(7)V99.
