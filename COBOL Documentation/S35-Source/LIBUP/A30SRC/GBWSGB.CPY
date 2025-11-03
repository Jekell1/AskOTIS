      * COPYMEMBER: LIBGB/GBWSGB
      *********************************************************
      *    LIBGB/GBWSGB: GBFILE PATH WORKERS
      *    /USR/DATA/XX/XX####/GB/GBFILE
      ******************************************************************
       01  GB-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  GB-PATH                   PIC X(35)   VALUE SPACES.
       01  GB-ALL-PATH.
           03  GB-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES GB-ALL-BASE.
               05  GB-ALL-BASE-USR   PIC X(05).
               05  GB-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GB-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GB-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/GB/GBFILE".
