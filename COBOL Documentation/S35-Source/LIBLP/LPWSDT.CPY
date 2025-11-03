      * COPYMEMBER: LIBLP/LPWSDT
      *********************************************************
      *        LIBLP/LPWSDT: DTFILE PATH WORKERS
      *    DL-PATH = /USR/DATA/XX/XX####/LP/DLFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      *********************************************************
       01  DT-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  DT-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES DT-PATH.
           03  DT-PATH-BEG           PIC X(13).
           03  DT-PATH-OWNST         PIC X(02).
           03  DT-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  DT-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES DT-PATH-FILE.
               05  FILLER            PIC X(03).
               05  DT-PATH-SQL       PIC X(12).

       01  DT-ALL-PATH.
           03  DT-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES DT-ALL-BASE.
               05  DT-ALL-BASE-USR   PIC X(05).
               05  DT-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DT-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DT-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/DTFILE".
