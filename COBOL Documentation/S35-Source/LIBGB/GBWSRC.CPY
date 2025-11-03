      * COPYMEMBER: LIBGB/GBWSRC
      *********************************************************
      *    LIBLP/LPWSSP: RCFILE PATH WORKERS
      *    /USR/DATA/XX/GB/RCFILE
      ******************************************************************
       01  RC-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  RC-PATH                   PIC X(35)   VALUE SPACES.

       01  FILLER                    REDEFINES RC-PATH.
           03  RC-PATH-BEG           PIC X(13).
           03  RC-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES RC-PATH-FILE.
               05  FILLER            PIC X(03).
               05  RC-PATH-SQL       PIC X(19).

       01  RC-ALL-PATH.
           03  RC-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES RC-ALL-BASE.
               05  RC-ALL-BASE-USR   PIC X(05).
               05  RC-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  RC-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/RCFILE".

       01  RCSUB                     PIC 9(2).
