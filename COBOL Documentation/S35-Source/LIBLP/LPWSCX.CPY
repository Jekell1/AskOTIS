      * COPYMEMBER: LIBLP/LPWSCX
      ******************************************************************
      *    LIBLP/LPWSCX: CXFILE PATH WORKERS
      *    CX-PATH = /USR/DATA/XX/XX####/LP/CXFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      ******************************************************************
       01  CX-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  CX-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES CX-PATH.
           03  CX-PATH-BEG           PIC X(13).
           03  CX-PATH-OWNST         PIC X(02).
           03  CX-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  CX-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES CX-PATH-FILE.
               05  FILLER            PIC X(03).
               05  CX-PATH-SQL       PIC X(12).

       01  CX-ALL-PATH.
           03  CX-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES CX-ALL-BASE.
               05  CX-ALL-BASE-USR   PIC X(05).
               05  CX-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CX-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CX-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/LP/CXFILE".
