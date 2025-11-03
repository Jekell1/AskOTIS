      * COPYMEMBER: LIBGB/GBWSSE
      *********************************************************
      *        LIBGB/GBWSSE: SEFILE PATH WORKERS
      *        /USR/DATA/XX/GB/SEFILE
      *********************************************************
       01  SE-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  SE-PATH                   PIC X(35)   VALUE SPACES.
       01  SE-ALL-PATH.
           03  SE-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES SE-ALL-BASE.
               05  SE-ALL-BASE-USR   PIC X(05).
               05  SE-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  SE-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/SEFILE".
