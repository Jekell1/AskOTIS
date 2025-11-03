      * COPYMEMBER: LIBCL/BISCANW
      *********************************************************************
      *   BISCANW
      *                 (CL)
      *   DESCRIPTION:  WORKERS FOR BISCAN.C
      *
      * REV:
      *  082097 JTG ADDED SRCD AND PRLNNO TO STOP THEM FROM
      *  121995 BAH DOING A "PB" PAYOFF, THEN BOOKING AN "NB" LOAN
      *             THEN REVERSING THE PB PAYOFF AND HAVING 2 OPEN LOANS
      *  170323 BAH CHANGED BISCAN-TRANS-DATE TO 9(8), YYYYMMDD
      * BAH 2024.0315 REMOVED BISCAN-LIASMLN AND BISCAN-PRLOANS
      *********************************************************************
       01  BISCAN-WORKERS.
           03  BISCAN-CALL-CODE      PIC 9.
           03  BISCAN-STATE-FG       PIC X.
           03  BISCAN-BRANCH-FG      PIC X.
           03  BISCAN-BRANCH         PIC 9(4).
           03  BISCAN-STATE          PIC XX.
           03  BISCAN-SSNO           PIC 9(9).
           03  BISCAN-TRANS-DATE     PIC 9(8).
           03  BISCAN-SRCD           PIC XX.
           03  BISCAN-PRLNNO         PIC 9(8).

