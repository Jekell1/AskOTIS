      * COPYMEMBER: LIBLP/LPWSLSG
      *********************************************************
      *        LIBLP/LPWSLSG: LSGFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LSGFILE
      *********************************************************
       01  LSG-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LSG-PATH                   PIC X(35)  VALUE SPACES.
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
