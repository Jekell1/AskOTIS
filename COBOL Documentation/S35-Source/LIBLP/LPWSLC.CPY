      * COPYMEMBER: LIBLP/LPWSLC
      *********************************************************
      *    LIBLP/LPWSLC: LCFILE PATH WORKERS
      *********************************************************
       01  LC-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  LC-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES LC-PATH.
           03  LC-PATH-BEG           PIC X(13).
           03  LC-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES LC-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LC-PATH-SQL       PIC X(19).

       01  LC-ALL-PATH.
           03  LC-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LC-ALL-BASE.
               05  LC-ALL-BASE-USR   PIC X(05).
               05  LC-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  LC-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/LP/LCFILE".
