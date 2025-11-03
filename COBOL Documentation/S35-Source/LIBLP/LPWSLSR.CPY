      * COPYMEMBER: LIBLP/LPWSLSR
      *********************************************************
      *        LIBLP/LPWSLSR: LSRFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LSRFILE
      *********************************************************
       01  LSR-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LSR-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LSR-PATH.
           03  LSR-PATH-BEG           PIC X(13).
           03  LSR-PATH-OWNST         PIC X(02).
           03  LSR-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LSR-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LSR-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LSR-PATH-SQL       PIC X(12).

       01  LSR-ALL-PATH.
           03  LSR-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES LSR-ALL-BASE.
               05  LSR-ALL-BASE-USR  PIC X(05).
               05  LSR-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSR-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSR-ALL-DATA          PIC X(06).
           03  FILLER                PIC X(11)  VALUE "/LP/LSRFILE".


       01  LSRSUB                    PIC 9(3).
