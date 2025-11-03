      * COPYMEMBER: LIBGB/RBACTW
      **************************************************************************
      *    REBATE CALCULATION WORKERS FOR RBACT FORMULA 'F' AND '0 6'
      *
      *REV:
      * JTG 091196 ADDED  RBACT-ELAPSED-UNITPER RBACT-ELAPSED-UNITPER-REM
      * JTG 021098 ADDED RBACT-EARN-FULL-MONTH, TFC #637
      *            RE: SP-EARNACCRL-FG() = 'B'
      * JTG 000104 ADDED RBACT-PENALTY-FG RE: SP-RBREDUC = 9999.04 WISCONSIN IB
      *            REAL ESTATE PREPAY PENALTY, WISCONSIN #1031
      * JTG 011026 CHANGED RBACT-APRATE TO RBACT-RATE FOR NEW REFUND FORMULA
      *            " 0  6 "                                      REGENCY PR#1631
      * BAH 161026 CHANGED RBDATE, REFDATE, INTDATE AND ORIG-1STPAY TO 9(8)
      **************************************************************************
      * INPUT FIELDS:
       01  RBACT-RBDATE           PIC 9(8)      COMP    VALUE 0.
       01  RBACT-REFDATE          PIC 9(8)      COMP    VALUE 0.
       01  RBACT-ORGTERM          PIC S9(5)     COMP    VALUE 0.
       01  RBACT-TOTCHG           PIC S9(6)V99  COMP    VALUE 0.
       01  RBACT-SKIPDEF-FG       PIC X                 VALUE " ".
       01  RBACT-EARN-FULL-MONTH  PIC X                 VALUE " ".
       01  RBACT-INTDATE          PIC 9(8)      COMP    VALUE 0.
       01  RBACT-ORIG-1STPAY      PIC 9(8)      COMP    VALUE 0.
       01  RBACT-TOTNODEF         PIC S9(3)     COMP    VALUE 0.
       01  RBACT-RATE             PIC 9(3)V9(3) COMP-3  VALUE 0.
       01  RBACT-REGPYAMT         PIC 9(6)V99   COMP-3  VALUE 0.
       01  RBACT-1STPYAMT         PIC 9(6)V99   COMP-3  VALUE 0.
       01  RBACT-LASTPYAMT        PIC S9(6)V99  COMP-3  VALUE 0.
       01  RBACT-UNITPER-CD       PIC X.
       01  RBACT-UNITPER-FREQ     PIC 999       COMP-3.
       01  RBACT-PENALTY-FG       PIC X.

      * OUTPUT WORKERS:
       01  RBACT-REBATE           PIC S9(7)V99    COMP-3.
       01  RBACT-PRES-VALUE       PIC S9(7)V99    COMP-3.

      * INTERNAL WORKERS:
       01  RBACT-ELAPSED-MONTHS      PIC S9(5)       COMP.
       01  RBACT-ELAPSED-REM         PIC S9(5)V9(10) COMP.
       01  RBACT-ELAPSED-DAYS        PIC S9(5)       COMP.
       01  RBACT-ELAPSED-UNITPER     PIC S9(5)       COMP.
       01  RBACT-ELAPSED-UNITPER-REM PIC S9(5)V9(10) COMP.
       01  RBACT-SUB                 PIC 99          COMP  VALUE 0.
       01  RBACT-SUB2                PIC 99          COMP  VALUE 0.
       01  RBACTF                    PIC S9V9(15)    COMP.
       01  RBACTF-ADJ                PIC S9V9(15)    COMP.
       01  RBACTT                    PIC S999        COMP.
       01  RBACTFP                   PIC S9(6)V99    COMP.
       01  RBACTRP                   PIC S9(6)V99    COMP.
       01  RBACTRP-A--N              PIC S999V9(15)  COMP.
       01  RBACTRP-N                 PIC S999        COMP.
       01  RBACTI                    PIC S999V9(15)  COMP.
       01  RBACTADJ                  PIC S999V9(15)  COMP.
       01  RBACTPV                   PIC S9(7)V99    COMP.
       01  RBACTLP-FAC               PIC S999V9(15)  COMP.
       01  RBACTLP-N                 PIC S999        COMP.
       01  RBACTLP                   PIC S9(6)V99    COMP.
       01  RBACT-FACTOR              PIC S9(3)V9(10) COMP.
