      * COPYMEMBER: LIBLP/LPWSBX
      *********************************************************
      *    BORROWER TRAILER FILE
      *
      *    LIBLP/LPWSBX: BXFILE PATH WORKERS
      *
      *    BX-PATH = /USR/DATA/XX/XX####/GB/BXFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      ******************************************************************
       01  BX-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  BX-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES BX-PATH.
           03  BX-PATH-BEG           PIC X(13).
           03  BX-PATH-OWNST         PIC X(02).
           03  BX-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  BX-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES BX-PATH-FILE.
               05  FILLER            PIC X(03).
               05  BX-PATH-SQL       PIC X(12).

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
