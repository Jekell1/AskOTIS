      * COPYMEMBER: LIBLP/LPWSLSG
      *********************************************************
      *        LIBLP/LPWSLSG: LSGFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LSGFILE
      *********************************************************
       01  LSG-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LSG-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LSG-PATH.
           03  LSG-PATH-BEG           PIC X(13).
           03  LSG-PATH-OWNST         PIC X(02).
           03  LSG-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LSG-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LSG-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LSG-PATH-SQL       PIC X(12).


       01  LSG-ALL-PATH.
           03  LSG-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES LSG-ALL-BASE.
               05  LSG-ALL-BASE-USR  PIC X(05).
               05  LSG-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSG-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSG-ALL-DATA          PIC X(06).
           03  FILLER                PIC X(11)  VALUE "/LP/LSGFILE".
