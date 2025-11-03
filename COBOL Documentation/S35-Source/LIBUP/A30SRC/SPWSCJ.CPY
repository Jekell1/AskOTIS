      * COPYMEMBER: LIBSP/SPWSCJ
      *********************************************************
      *        LIBSP/SPWSCJ: CJFILE PATH WORKERS
      *********************************************************
       01  CJ-PATH-OVERRIDE    PIC X(01) VALUE SPACES.
       01  CJ-PATH             PIC X(35) VALUE SPACES.

       01  CJ-ALL-PATH.
           03  CJ-ALL-BASE     PIC X(09).
           03  FILLER          REDEFINES CJ-ALL-BASE.
               05  CJ-ALL-BASE-USR   PIC X(05).
               05  CJ-ALL-BASE-DIR   PIC X(04).
           03  FILLER          PIC X     VALUE "/".
           03  CJ-ALL-MACHINE  PIC XX.
           03  FILLER          PIC X     VALUE "/".
           03  CJ-ALL-DATA     PIC X(6).
           03  FILLER          PIC X(10) VALUE "/SP/CJFILE".
