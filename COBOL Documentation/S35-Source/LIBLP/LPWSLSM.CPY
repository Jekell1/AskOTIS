      * COPYMEMBER: LIBLP/LPWSLSM
      *********************************************************
      *        LIBLP/LPWSLSM: LSMFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LSMFILE
      *********************************************************
       01  LSM-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LSM-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LSM-PATH.
           03  LSM-PATH-BEG           PIC X(13).
           03  LSM-PATH-OWNST         PIC X(02).
           03  LSM-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LSM-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LSM-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LSM-PATH-SQL       PIC X(12).

       01  LSM-ALL-PATH.
           03  LSM-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES LSM-ALL-BASE.
               05  LSM-ALL-BASE-USR  PIC X(05).
               05  LSM-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSM-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSM-ALL-DATA          PIC X(06).
           03  FILLER                PIC X(11)  VALUE "/LP/LSMFILE".
