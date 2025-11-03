      * COPYMEMBER: LIBGB/GBWSGR
      *********************************************************
      *        LIBGB/GBWSGR: GRFILE PATH WORKERS
      *        /USR/DATA/XX/GB/GRFILE
      *********************************************************
       01  GR-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  GR-PATH                   PIC X(35)   VALUE SPACES.
       01  GR-ALL-PATH.
           03  GR-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES GR-ALL-BASE.
               05  GR-ALL-BASE-USR   PIC X(05).
               05  GR-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GR-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/GRFILE".
