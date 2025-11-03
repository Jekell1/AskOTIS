      * COPYMEMBER: LIBLP/LPWSCDB
      *********************************************************
      *    LIBLP/LPWSCDB: CDBFILE PATH WORKERS
      *    /USR/DATA/XX/LP/CDBFILE
      ******************************************************************
       01  CDB-PATH-OVERRIDE         PIC X(01)   VALUE SPACES.

       01  CDB-PATH                  PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES CDB-PATH.
           03  CDB-PATH-BEG          PIC X(13).
           03  CDB-PATH-FILE         PIC X(22).
           03  FILLER                REDEFINES CDB-PATH-FILE.
               05  FILLER            PIC X(03).
               05  CDB-PATH-SQL      PIC X(19).

       01  CDB-ALL-PATH.
           03  CDB-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES CDB-ALL-BASE.
               05  CDB-ALL-BASE-USR  PIC X(05).
               05  CDB-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CDB-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/LP/CDBFILE".
