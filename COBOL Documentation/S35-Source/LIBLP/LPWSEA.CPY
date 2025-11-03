      * COPYMEMBER: LIBLP/LPWSEA
      *********************************************************
      *        LIBLP/LPWSEA: EAFILE PATH WORKERS
      * EA-PATH = /USR/DATA/R#/GB/EAFILE
      *           |----BEG----||-----FILE----|
      *                           |---SQL----|
      *
      *********************************************************
       01  EA-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  EA-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES EA-PATH.
           03  EA-PATH-BEG           PIC X(13).
           03  EA-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES EA-PATH-FILE.
               05  FILLER            PIC X(03).
               05  EA-PATH-SQL       PIC X(19).

       01  EA-ALL-PATH.
           03  EA-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES EA-ALL-BASE.
               05  EA-ALL-BASE-USR   PIC X(05).
               05  EA-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  EA-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/EAFILE".
