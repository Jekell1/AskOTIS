      * COPYMEMBER: LIBLP/LPWSNO
      *********************************************************
      *        LIBLP/LPWSNO: NOFILE PATH WORKERS
      *    NO-PATH = /USR/DATA/XX/XX####/LP/NOFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      *********************************************************
       01  NO-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  NO-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES NO-PATH.
           03  NO-PATH-BEG           PIC X(13).
           03  NO-PATH-OWNST         PIC X(02).
           03  NO-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  NO-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES NO-PATH-FILE.
               05  FILLER            PIC X(03).
               05  NO-PATH-SQL       PIC X(12).

       01  NO-ALL-PATH.
           03  NO-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES NO-ALL-BASE.
               05  NO-ALL-BASE-USR   PIC X(05).
               05  NO-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  NO-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  NO-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/NOFILE".
