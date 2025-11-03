      * COPYMEMBER: LIBLP/PAYOFXW
      **************************************************************************
      *   PAYOFXW
      *                 (LIBLP)
      *   DESCRIPTION:  PRIOR LOAN PAYOFF FIELDS
      *
      * REV:
      *  JTG 051089 CHANGED WORKERS TO COMP
      *  JTG 061989 CHANGED PRIORS TO 9(5)V99
      *  JTG 081789 ADDED PASS-CDEL-ARREARAGE (GA)
      *  BAH 090189 CHANGED PRLNNO TO 8
      *  BAH 120696 ADDED PASS-SSNO, NEEDED FOR AUTO RENEWAL IN XONPC0,
      *             TO SEE IF MAKERS ARE DIFFERENT !!
      *  JTG 061998 ADDED 88 FREE-ERROR-STATUS, A NEW PRIOR-LOAN-NOT-VALID
      *             CODE 'J', WHICH WILL BE TESTED IN LOAN POSTING PROGRAMS
      *  BAH 030619 ADDED PASS-PRIOR-SUB-CLASS, LENDMARK PR# 2101
      *  BLM 160622 REMOVED PASS-LN-MORELN, PD0003
      *  BAH 160720 INCREASED DATE FIELDS
      *   CS 160811 CHANGED PASS-PRIOR-LOAN-CLASS FROM PIC9(2) TO 9(3)
      **************************************************************************
       01  PASS-PAYOFF-FLDS.
           03  PASS-INPUT-FLDS.
               05  PASS-PRLNNO                  PIC 9(8)       COMP.
               05  PASS-TRANS-DATE              PIC 9(8)       COMP.
               05  PASS-CURRENT-PD-SRCD         PIC XX.
           03  PASS-OUTPUT-FLDS.
               05  PASS-PRIOR-LOAN-STATE        PIC XX.
               05  PASS-PRIOR-LOAN-CLASS        PIC 999        COMP.
               05  PASS-PRIOR-LOAN-DATE         PIC 9(8)       COMP.
               05  PASS-PRBAL                   PIC S9(7)V99   COMP.
               05  PASS-PRINTDUE                PIC S9(7)V99   COMP.
               05  PASS-PRFINCHGRB              PIC S9(7)V99   COMP.
               05  PASS-PRINSRB                 PIC S9(7)V99   COMP.
               05  PASS-STAT-FG                 PIC X.
                   88 PRIOR-LOAN-NOT-POSTED     VALUE "Y".
                   88 ERROR-IN-PAYOFF           VALUE "L" "S".
                   88 FREE-ERROR-STATUS         VALUE "J".
                   88 PRIOR-LOAN-NOT-VALID      VALUE "Z" "S" "N" "J".
               05  PASS-INT-REB                 PIC S9(7)V99 COMP.
               05  PASS-SC-REB                  PIC S9(7)V99 COMP.
               05  PASS-MF-REB                  PIC S9(7)V99 COMP.
               05  PASS-NETDUE                  PIC S9(7)V99 COMP.
               05  PASS-CDEL-ARREARAGE          PIC S9(7)V99 COMP.
               05  PASS-SSNO                    PIC 9(9)     COMP.
               05  PASS-PRIOR-SUB-CLASS         PIC 99       COMP.
               05  FILLER                       PIC X(25).
