      * COPYMEMBER: LIBLP/LPWSDL
      *********************************************************
      *    LIBLP/LPWSDL: DLFILE PATH WORKERS
      *    DL-PATH = /USR/DATA/XX/XX####/LP/DLFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      ******************************************************************
       01  DL-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  DL-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES DL-PATH.
           03  DL-PATH-BEG           PIC X(13).
           03  DL-PATH-OWNST         PIC X(02).
           03  DL-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  DL-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES DL-PATH-FILE.
               05  FILLER            PIC X(03).
               05  DL-PATH-SQL       PIC X(12).

       01  DL-ALL-PATH.
           03  DL-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES DL-ALL-BASE.
               05  DL-ALL-BASE-USR   PIC X(05).
               05  DL-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  DL-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  DL-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/LP/DLFILE".
