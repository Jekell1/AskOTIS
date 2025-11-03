      * COPYMEMBER: LIBLP/LPWSBX
      *********************************************************
      *    BORROWER TRAILER FILE
      *    LIBLP/LPWSBX: BXFILE PATH WORKERS
      *    /USR/DATA/XX/XX####/GB/BXFILE
      ******************************************************************
       01  BX-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  BX-PATH                   PIC X(35)   VALUE SPACES.
       01  BX-ALL-PATH.
           03  BX-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES BX-ALL-BASE.
               05  BX-ALL-BASE-USR   PIC X(05).
               05  BX-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  BX-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  BX-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/GB/BXFILE".
