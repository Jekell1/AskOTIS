      * COPYMEMBER: LIBLP/LPWSPA
      *********************************************************
      *        LIBLP/LPWSPA: PAFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/PAFILE
      *
      *********************************************************
       01  PA-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  PA-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES PA-PATH.
           03  PA-PATH-BEG           PIC X(13).
           03  PA-PATH-OWNST         PIC X(02).
           03  PA-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  PA-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES PA-PATH-FILE.
               05  FILLER            PIC X(03).
               05  PA-PATH-SQL       PIC X(12).

       01  PA-ALL-PATH.
           03  PA-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES PA-ALL-BASE.
               05  PA-ALL-BASE-USR   PIC X(05).
               05  PA-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  PA-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  PA-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/PAFILE".
