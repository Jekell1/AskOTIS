      * COPYMEMBER: LIBCL/PAYOFFW
      **************************************************************************
      *   PAYOFFW
      *                 (CL)
      *   DESCRIPTION:  CALLED LOAN PAYOFF FIELDS
      *
      *  REV:
      *  JTG 061998 ADDED 88 FREE-ERROR-STATUS, A NEW PRIOR-LOAN-NOT-VALID
      *             CODE 'J', WHICH WILL BE TESTED IN LOAN POSTING PROGRAMS
      *  BAH 161026 CHANGED PAYOFF-PAYDATE TO 9(8) FROM 9(6)
      **************************************************************************
       01  PAYOFF-WORKERS.
      * INPUT FIELDS:
           03  PAYOFF-ACCTNO                PIC 9(8) COMP.
           03  PAYOFF-PAYDATE               PIC 9(8) COMP.
           03  PAYOFF-SRCD                  PIC XX.
           03  PAYOFF-MAKERCD               PIC X.
           03  PAYOFF-DEF-STOP              PIC X.

      * OUTPUT FIELDS:
           03  PAYOFF-CDEL-ARREARAGE        PIC S9(7)V99 COMP.
           03  PAYOFF-AVAILCREDIT           PIC S9(7)V99 COMP.
           03  PAYOFF-STAT-FG               PIC X.
               88 PRIOR-LOAN-NOT-POSTED     VALUE "Y".
               88 ERROR-IN-PAYOFF           VALUE "L" "S".
               88 FREE-ERROR-STATUS         VALUE "J".
               88 PRIOR-LOAN-NOT-VALID      VALUE "Z" "S" "N" "J".
