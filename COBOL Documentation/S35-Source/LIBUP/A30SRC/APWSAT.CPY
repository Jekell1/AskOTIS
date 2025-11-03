      * COPYMEMBER: LIBAP/APWSAT
      ******************************************************************
      *    LIBAP/APWSAT: ATFILE PATH WORKERS
      *    /USR/DATA/R#/AP/ATFILE
      ******************************************************************
       01  AT-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  AT-PATH                   PIC X(35)   VALUE SPACES.
       01  AT-ALL-PATH.
           03  AT-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES AT-ALL-BASE.
               05  AT-ALL-BASE-USR   PIC X(05).
               05  AT-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AT-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/AP/ATFILE".
