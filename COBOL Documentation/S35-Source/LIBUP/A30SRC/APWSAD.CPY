      * COPYMEMBER: LIBAP/APWSAD
      ******************************************************************
      *    LIBAP/APWSAD: ADFILE PATH WORKERS
      *    /USR/DATA/R#/AP/ADFILE
      ******************************************************************
       01  AD-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  AD-PATH                   PIC X(35)   VALUE SPACES.
       01  AD-ALL-PATH.
           03  AD-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES AD-ALL-BASE.
               05  AD-ALL-BASE-USR   PIC X(05).
               05  AD-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AD-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/AP/ADFILE".
