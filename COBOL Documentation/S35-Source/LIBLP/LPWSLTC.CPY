      * COPYMEMBER: LIBLP/LPWSLTC
      *********************************************************
      *        LIBLP/LPWSLTC: COLLATERAL PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LTCFILE
      *
      * LTC-PATH = /USR/DATA/R#/XX####/LP/LTCFILE
      *            |----BEG----||-MID-||-----FILE----|
      *                                   |---SQL----|
      *
      *********************************************************
       01  LTC-PATH-OVERRIDE   PIC X(01)  VALUE SPACES.
       01  LTC-PATH            PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LTC-PATH.
           03  LTC-PATH-BEG          PIC X(13).
           03  LTC-PATH-OWNST        PIC X(02).
           03  LTC-PATH-OWNBR        PIC 9(04).
           03  FILLER                PIC X(01).
           03  LTC-PATH-FILE         PIC X(15).
           03  FILLER                REDEFINES LTC-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LTC-PATH-SQL      PIC X(12).

       01  LTC-ALL-PATH.
           03  LTC-ALL-BASE           PIC X(09).
           03  FILLER REDEFINES LTC-ALL-BASE.
               05  LTC-ALL-BASE-USR   PIC X(05).
               05  LTC-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTC-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTC-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)  VALUE "/LP/LTCFILE".

       01  LTCSUB                    PIC 9(3).
