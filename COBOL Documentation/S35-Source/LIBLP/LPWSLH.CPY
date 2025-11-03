      * COPYMEMBER: LIBLP/LPWSLH
      *********************************************************
      *        LIBLP/LPWSLH: LHFILE PATH WORKERS
      *
      *    LH-PATH = /USR/DATA/XX/XX####/LP/LHFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      *********************************************************
       01  LH-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  LH-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LH-PATH.
           03  LH-PATH-BEG           PIC X(13).
           03  LH-PATH-OWNST         PIC X(02).
           03  LH-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LH-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LH-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LH-PATH-SQL       PIC X(12).

       01  LH-ALL-PATH.
           03  LH-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LH-ALL-BASE.
               05  LH-ALL-BASE-USR   PIC X(05).
               05  LH-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LH-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LH-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/LHFILE".
