      * COPYMEMBER: LIBLP/LPWSPD
      *********************************************************
      *        LIBLP/LPWSPD: PDFILE PATH WORKERS
      * PD-PATH = /USR/DATA/R#/XX####/LP/PDFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      *********************************************************
       01  PD-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  PD-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES PD-PATH.
           03  PD-PATH-BEG           PIC X(13).
           03  PD-PATH-OWNST         PIC X(02).
           03  PD-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  PD-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES PD-PATH-FILE.
               05  FILLER            PIC X(03).
               05  PD-PATH-SQL       PIC X(12).

       01  PD-ALL-PATH.
           03  PD-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES PD-ALL-BASE.
               05  PD-ALL-BASE-USR   PIC X(05).
               05  PD-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  PD-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  PD-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/PDFILE".
