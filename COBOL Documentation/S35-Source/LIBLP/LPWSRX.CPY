      * COPYMEMBER: LIBLP/LPWSRX
      ******************************************************************
      *        LIBLP/LPWSRX: RXFILE PATH WORKERS
      *    RX-PATH = /USR/DATA/XX/XX####/LP/RXFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      ******************************************************************
       01  RX-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  RX-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES RX-PATH.
           03  RX-PATH-BEG           PIC X(13).
           03  RX-PATH-OWNST         PIC X(02).
           03  RX-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  RX-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES RX-PATH-FILE.
               05  FILLER            PIC X(03).
               05  RX-PATH-SQL       PIC X(12).

       01  RX-ALL-PATH.
           03  RX-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES RX-ALL-BASE.
               05  RX-ALL-BASE-USR   PIC X(05).
               05  RX-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  RX-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  RX-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/RXFILE".
