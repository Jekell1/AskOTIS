      * COPYMEMBER: LIBLP/LPWSTY
      *********************************************************
      *        LIBLP/LPWSTY: TYFILE PATH WORKERS
      *
      *    TY-PATH = /USR/DATA/XX/XX####/LP/TYFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      *********************************************************
       01  TY-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  TY-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES TY-PATH.
           03  TY-PATH-BEG           PIC X(13).
           03  TY-PATH-OWNST         PIC X(02).
           03  TY-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  TY-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES TY-PATH-FILE.
               05  FILLER            PIC X(03).
               05  TY-PATH-SQL       PIC X(12).

       01  TY-ALL-PATH.
           03  TY-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TY-ALL-BASE.
               05  TY-ALL-BASE-USR   PIC X(05).
               05  TY-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  TY-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  TY-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/TYFILE".
