      * COPYMEMBER: LIBLP/LPWSBI
      *********************************************************
      *    LIBLP/LPWSBI: BIFILE PATH WORKERS
      *
      * BI-PATH = /USR/DATA/SH/BIFILE
      *           |----BEG----||-----FILE----|
      *                        |-----SQL-----|
      *
      *********************************************************
       01  BI-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  BI-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES BI-PATH.
           03  BI-PATH-BEG           PIC X(13).
           03  BI-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES BI-PATH-FILE.
               05  BI-PATH-SQL       PIC X(22).

       01  BI-ALL-PATH.
           03  BI-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES BI-ALL-BASE.
               05  BI-ALL-BASE-USR   PIC X(05).
               05  BI-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(26)   VALUE "/SH/BIFILE".

