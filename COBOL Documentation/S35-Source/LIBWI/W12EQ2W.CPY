      * COPYMEMBER: LIBWI/W12EQ2W

      **************************************************************************
      *                   LIBWI/W12EQ2W
      *
      *     WORKERS FOR XONPC0.C AND LONPC2.C
      *
      * JTG 030430 CREATED FORMULA 12 LIKE 2 BUT SPREADS EXTENSION CHARGE
      *            OVER PAYMENTS  88  SP-PYEXFRMLA-SAME2-SPREAD VALUE 12.
      *                                                           CITIZENS #2006
      **************************************************************************
       01  W12EQ2-BUF.
           03  W12EQ2-OVRDRATE       PIC 9(3)V9(3).
           03  W12EQ2-OVRDRATE-FG    PIC X.
           03  W12EQ2-EFFRATE        PIC 9(3)V9(3).
           03  W12EQ2-APRATE         PIC 9(3)V9(3).
           03  W12EQ2-ORGTERM        PIC 9(3).
           03  W12EQ2-EXTCHG         PIC 9(6)V99.
           03  W12EQ2-SPREAD         PIC 9(6)V99.
           03  W12EQ2-1STPYAMT       PIC 9(6)V99.
           03  W12EQ2-REGPYAMT       PIC 9(6)V99.
           03  W12EQ2-LASTPYAMT      PIC 9(6)V99.
           03  W12EQ2-REM            PIC 9(6)V99.
