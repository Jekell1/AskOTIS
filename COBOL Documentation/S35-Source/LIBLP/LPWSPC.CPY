      * COPYMEMBER: LIBLP/LPWSPC
      *********************************************************
      *        LIBLP/LPWSPC: PCFILE PATH WORKERS
      * PC-PATH = /USR/DATA/R#/XX####/LP/PCFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      *********************************************************
       01  PC-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  PC-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES PC-PATH.
           03  PC-PATH-BEG           PIC X(13).
           03  PC-PATH-OWNST         PIC X(02).
           03  PC-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  PC-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES PC-PATH-FILE.
               05  FILLER            PIC X(03).
               05  PC-PATH-SQL       PIC X(12).

       01  PC-ALL-PATH.
           03  PC-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES PC-ALL-BASE.
               05  PC-ALL-BASE-USR   PIC X(05).
               05  PC-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  PC-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  PC-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/PCFILE".


       01  PCSUB                     PIC 9(2).
