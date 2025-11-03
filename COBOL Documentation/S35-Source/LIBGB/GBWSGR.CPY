      * COPYMEMBER: LIBGB/GBWSGR
      *********************************************************
      *        LIBGB/GBWSGR: GRFILE PATH WORKERS
      *
      * GR-PATH = /USR/DATA/R#/GB/GRFILE
      *           |----BEG----||-----FILE----|
      *                           |---SQL----|
      *
      *********************************************************
       01  GR-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  GR-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES GR-PATH.
           03  GR-PATH-BEG           PIC X(13).
           03  GR-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES GR-PATH-FILE.
               05  FILLER            PIC X(03).
               05  GR-PATH-SQL       PIC X(19).

       01  GR-ALL-PATH.
           03  GR-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES GR-ALL-BASE.
               05  GR-ALL-BASE-USR   PIC X(05).
               05  GR-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GR-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/GRFILE".

