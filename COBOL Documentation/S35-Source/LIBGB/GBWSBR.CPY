      * COPYMEMBER: LIBGB/GBWSBR
      *********************************************************
      *    LIBGB/LPWSBR:       BRFILE PATH WORKERS
      *
      * BR-PATH = /USR/DATA/R#/GB/BRFILE
      *           |----BEG----||-----FILE----|
      *                           |---SQL----|
      *
      *
      ******************************************************************
       01  BR-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  BR-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES BR-PATH.
           03  BR-PATH-BEG           PIC X(13).
           03  BR-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES BR-PATH-FILE.
               05  FILLER            PIC X(03).
               05  BR-PATH-SQL       PIC X(19).

       01  BR-ALL-PATH.
           03  BR-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES BR-ALL-BASE.
               05  BR-ALL-BASE-USR   PIC X(05).
               05  BR-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  BR-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/BRFILE".
