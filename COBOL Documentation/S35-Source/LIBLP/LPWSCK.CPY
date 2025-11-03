      * COPYMEMBER: LIBLP/LPWSCK
      *********************************************************
      *        LIBLP/LPWSCK: CKFILE PATH WORKERS
      *    CK-PATH = /USR/DATA/XX/XX####/LP/CKFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      *********************************************************
       01  CK-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  CK-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES CK-PATH.
           03  CK-PATH-BEG           PIC X(13).
           03  CK-PATH-OWNST         PIC X(02).
           03  CK-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  CK-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES CK-PATH-FILE.
               05  FILLER            PIC X(03).
               05  CK-PATH-SQL       PIC X(12).

       01  CK-ALL-PATH.
           03  CK-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES CK-ALL-BASE.
               05  CK-ALL-BASE-USR   PIC X(05).
               05  CK-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  CK-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  CK-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/CKFILE".
