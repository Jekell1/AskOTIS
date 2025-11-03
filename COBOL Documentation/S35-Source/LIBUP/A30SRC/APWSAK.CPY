      * COPYMEMBER: LIBAP/APWSAK
      ******************************************************************
      *    LIBAP/APWSAK: AKFILE PATH WORKERS
      *    /USR/DATA/R#/AP/AKFILE
      ******************************************************************
       01  AK-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  AK-PATH                   PIC X(35)   VALUE SPACES.
       01  AK-ALL-PATH.
           03  AK-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES AK-ALL-BASE.
               05  AK-ALL-BASE-USR   PIC X(05).
               05  AK-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AK-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/AP/AKFILE".
