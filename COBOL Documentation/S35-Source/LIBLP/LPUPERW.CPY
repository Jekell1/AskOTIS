      * COPYMEMBER: LIBLP/LPUPERW
      **************************************************************************
      *    LPUPERW
      *    UNIT PERIOD WORKERS (APR RELATED)
      *
      * REV:
      *  JTG 060504 CHANGED LOGIC FOR SINGLE PAYS LOANS > 11 MONTHS
      *             TO YIELD REMAINING DAYS FROM UPER-INTDATE PLUS
      *             FULL UNIT PERIODS TO UPER-1STPYDATE
      *             ADDED NEW LOAN CALCULATION FOR RICHARD        LENDMARK #2709
      *             INCREASED UPER-UNITPER-PER-YEAR TO 9(3)V9(15)
      *             AND UPER-FRACT-UNITPER TO V9(18)
      **************************************************************************
       01  UPER-WORK-AREA.
           03  UPER-OUTPUT-AREA.
               05  UPER-UNITPER-PER-YEAR   PIC 9(3)V9(15)  COMP-3.
               05  UPER-DAYS-IN-UNITPER    PIC 9(3)       COMP-3.
               05  UPER-FULL-UNITPER       PIC 9(3)       COMP-3.
               05  UPER-FRACT-UNITPER      PIC V9(18)      COMP-3.
               05  UPER-ERRCD              PIC XX.
           03  UPER-INPUT-AREA.
               05  UPER-UNITPER-CD         PIC X.
               05  UPER-UNITPER-FREQ       PIC 9(3)       COMP-3.
               05  UPER-INTDATE            PIC 9(8)       COMP-3.
               05  UPER-1STPYDATE          PIC 9(8)       COMP-3.
               05  UPER-TERM               PIC 9(3)       COMP-3.
           03  UPER-WORKERS.
               05  UPER-REUBEN             PIC 9V9(9)     COMP-3.
               05  UPER-SINGLE-PAY-CD      PIC X.
               05  UPER-SINGLE-PAY-FREQ    PIC 9(3)       COMP-3.
