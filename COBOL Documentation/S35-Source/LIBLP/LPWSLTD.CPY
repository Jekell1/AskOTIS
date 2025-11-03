      * COPYMEMBER: LIBLP/LPWSLTD
      *********************************************************
      *        LIBLP/LPWSLTD: DEBIT CARD PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LTDFILE
      *********************************************************
       01  LTD-PATH-OVERRIDE   PIC X(01)  VALUE SPACES.
       01  LTD-PATH            PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LTD-PATH.
           03  LTD-PATH-BEG           PIC X(13).
           03  LTD-PATH-OWNST         PIC X(02).
           03  LTD-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  LTD-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LTD-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LTD-PATH-SQL       PIC X(12).

       01  LTD-ALL-PATH.
           03  LTD-ALL-BASE           PIC X(09).
           03  FILLER REDEFINES LTD-ALL-BASE.
               05  LTD-ALL-BASE-USR   PIC X(05).
               05  LTD-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTD-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTD-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)  VALUE "/LP/LTDFILE".
