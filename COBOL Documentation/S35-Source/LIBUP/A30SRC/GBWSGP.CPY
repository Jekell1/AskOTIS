      * COPYMEMBER: LIBGB/GBWSGP
      *********************************************************
      *    LIBGB/LPWSGP:      GPFILE PATH WORKERS
      *    /USR/DATA/XX/GB/GPFILE
      ******************************************************************
       01  GP-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  GP-PATH                   PIC X(35)   VALUE SPACES.
       01  GP-ALL-PATH.
           03  GP-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES GP-ALL-BASE.
               05  GP-ALL-BASE-USR   PIC X(05).
               05  GP-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GP-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/GPFILE".
