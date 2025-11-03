      * COPYMEMBER: LIBGB/GBWSPW
      *********************************************************
      *        LIBGB/GBWSPW: PWFILE PATH WORKERS
      *        /USR/DATA/XX/GB/PWFILE
      *********************************************************
       01  PW-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  PW-PATH                   PIC X(35)   VALUE SPACES.
       01  PW-ALL-PATH.
           03  PW-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES PW-ALL-BASE.
               05  PW-ALL-BASE-USR   PIC X(05).
               05  PW-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  PW-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/PWFILE".
