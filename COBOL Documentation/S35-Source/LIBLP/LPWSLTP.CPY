      * COPYMEMBER: LIBLP/LPWSLTP
      *********************************************************
      *        LIBLP/LPWSLTP: PAYMENT SCHEDULE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LTPFILE
      *********************************************************
       01  LTP-PATH-OVERRIDE   PIC X(01)  VALUE SPACES.
       01  LTP-PATH            PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LTP-PATH.
           03  LTP-PATH-BEG           PIC X(13).
           03  LTP-PATH-OWNST         PIC X(02).
           03  LTP-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LTP-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LTP-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LTP-PATH-SQL       PIC X(12).

       01  LTP-ALL-PATH.
           03  LTP-ALL-BASE           PIC X(09).
           03  FILLER REDEFINES LTP-ALL-BASE.
               05  LTP-ALL-BASE-USR   PIC X(05).
               05  LTP-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTP-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTP-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)  VALUE "/LP/LTPFILE".

       01  LTPSUB                    PIC 9(3).
