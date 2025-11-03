      * COPYMEMBER: LIBGB/GBWSEO
      *********************************************************
      *        LIBGB/GBWSEO: EOFILE PATH WORKERS
      *        /USR/DATA/XX/GB/EOFILE
      *********************************************************
       01  EO-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  EO-PATH                   PIC X(35)   VALUE SPACES.
       01  EO-ALL-PATH.
           03  EO-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES EO-ALL-BASE.
               05  EO-ALL-BASE-USR   PIC X(05).
               05  EO-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  EO-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/EOFILE".
