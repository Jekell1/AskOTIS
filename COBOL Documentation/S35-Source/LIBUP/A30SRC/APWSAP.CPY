      * COPYMEMBER: LIBAP/APWSAP
      ******************************************************************
      *    LIBAP/APWSAP: APFILE PATH WORKERS
      *    /USR/DATA/R#/AP/APFILE
      ******************************************************************
       01  AP-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  AP-PATH                   PIC X(35)   VALUE SPACES.
       01  AP-ALL-PATH.
           03  AP-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES AP-ALL-BASE.
               05  AP-ALL-BASE-USR   PIC X(05).
               05  AP-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AP-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/AP/APFILE".
