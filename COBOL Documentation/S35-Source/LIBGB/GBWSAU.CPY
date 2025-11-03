      * COPYMEMBER: LIBGB/GBWSAU
      *********************************************************
      *    LIBGB/LPWSAU:       AUFILE PATH WORKERS (AUDIT/CHANGES)
      *
      * AU-PATH = /USR/DATA/R#/GB/AUFILE
      *           |----BEG----||-----FILE----|
      *                           |---SQL----|
      *
      ******************************************************************
       01  AU-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  AU-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES AU-PATH.
           03  AU-PATH-BEG           PIC X(13).
           03  AU-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES AU-PATH-FILE.
               05  FILLER            PIC X(03).
               05  AU-PATH-SQL       PIC X(19).

       01  AU-ALL-PATH.
           03  AU-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES AU-ALL-BASE.
               05  AU-ALL-BASE-USR   PIC X(05).
               05  AU-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AU-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/AUFILE".
