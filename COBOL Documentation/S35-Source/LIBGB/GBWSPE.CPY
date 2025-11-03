      * COPYMEMBER: LIBGB/GBWSPE
      *********************************************************
      *        LIBGB/GBWSPE: PEFILE PATH WORKERS
      *        /USR/DATA/XX/GB/PEFILE
      *********************************************************
       01  PE-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  PE-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES PE-PATH.
           03  PE-PATH-BEG           PIC X(13).
           03  PE-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES PE-PATH-FILE.
               05  FILLER            PIC X(03).
               05  PE-PATH-SQL       PIC X(19).

       01  PE-ALL-PATH.
           03  PE-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES PE-ALL-BASE.
               05  PE-ALL-BASE-USR   PIC X(05).
               05  PE-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  PE-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/PEFILE".
