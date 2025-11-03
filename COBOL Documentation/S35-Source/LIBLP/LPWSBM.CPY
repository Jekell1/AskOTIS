      * COPYMEMBER: LIBLP/LPWSBM
      *********************************************************
      *        LIBLP/LPWSBM: BMFILE PATH WORKERS
      *
      * BM-PATH = /USR/DATA/XX/XX####/LP/BMFILE
      *           |----BEG----||-MID-||-----FILE----|
      *
      *********************************************************
       01  BM-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  BM-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES BM-PATH.
           03  BM-PATH-BEG           PIC X(13).
           03  BM-PATH-OWNST         PIC X(02).
           03  BM-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  BM-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES BM-PATH-FILE.
               05  FILLER            PIC X(03).
               05  BM-PATH-SQL       PIC X(12).

       01  BM-ALL-PATH.
           03  BM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES BM-ALL-BASE.
               05  BM-ALL-BASE-USR   PIC X(05).
               05  BM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  BM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  BM-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/BMFILE".


       01  WS-BM-LTOUCH-TIME         PIC 9(8).
       01  FILLER REDEFINES WS-BM-LTOUCH-TIME.
           03  WS-BM-LTOUCH-HHMM     PIC 9(4).
           03  FILLER                PIC 9(4).
