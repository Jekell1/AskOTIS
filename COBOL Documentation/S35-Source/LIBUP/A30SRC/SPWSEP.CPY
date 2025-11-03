      * COPYMEMBER: LIBSP/SPWSEP  LOAN ELECTRONIC PAYMENT FILE
      *********************************************************
      *        LIBSP/SPWSEP: EPFILE PATH WORKERS
      *********************************************************
       01  EP-PATH-OVERRIDE    PIC X     VALUE SPACES.
       01  EP-PATH             PIC X(35) VALUE SPACES.
       01  EP-ALL-PATH.
           03  EP-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES EP-ALL-BASE.
               05  EP-ALL-BASE-USR   PIC X(05).
               05  EP-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  EP-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/SP/EPFILE".
