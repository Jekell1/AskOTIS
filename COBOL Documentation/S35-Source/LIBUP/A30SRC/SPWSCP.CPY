      * COPYMEMBER: LIBSP/SPWSCP
      ********************************************************************
      *  LIBSP/SPWSCP: CPFILE PATH WORKERS
      *  REV:
      *  BLV 112800 ADDED CP-GLOBAL-PATH; IF USING GLOBAL COLLECTOR
      *             QUEUING, EXTRACTION MAINTENANCES & RUNS MAY USE
      *             GLOBAL COLLECTOR POOL FILE IF BRANCH HAS GLOBAL
      *             QUEUING TURNED ON - THE ACTUAL FILE LAYOUT & USAGE
      *             WILL BE IDENTICAL, FILE IS JUST IN A DIFFERENT SPOT
      ********************************************************************
       01  CP-PATH-OVERRIDE    PIC X     VALUE SPACES.
       01  CP-PATH             PIC X(35) VALUE SPACES.
       01  CP-ALL-PATH.
           03  CP-ALL-BASE     PIC X(9).
           03  FILLER          REDEFINES CP-ALL-BASE.
               05  CP-ALL-BASE-USR   PIC X(05).
               05  CP-ALL-BASE-DIR   PIC X(04).
           03  FILLER          PIC X     VALUE "/".
           03  CP-ALL-MACHINE  PIC XX.
           03  FILLER          PIC X     VALUE "/".
           03  CP-ALL-DATA     PIC X(6).
           03  FILLER          PIC X(10) VALUE "/SP/CPFILE".

       01  CP-GLOBAL-PATH.
           03  CP-GLOBAL-BASE  PIC X(9).
           03  FILLER          PIC X     VALUE "/".
           03  CP-GLOBAL-MACHINE  PIC XX.
           03  FILLER          PIC X(10) VALUE "/SP/CPFILE".

