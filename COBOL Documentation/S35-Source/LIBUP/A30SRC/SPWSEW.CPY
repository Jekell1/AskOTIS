      * COPYMEMBER: LIBSP/SPWSEW
      *********************************************************
      *        LIBSP/SPWSEW: EWFILE PATH WORKERS
      *********************************************************
       01  EW-PATH-OVERRIDE    PIC X     VALUE SPACES.
       01  EW-PATH             PIC X(35) VALUE SPACES.

       01  EW-ALL-PATH.
           03  EW-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES EW-ALL-BASE.
               05  EW-ALL-BASE-USR   PIC X(05).
               05  EW-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  EW-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/SP/EWFILE".
