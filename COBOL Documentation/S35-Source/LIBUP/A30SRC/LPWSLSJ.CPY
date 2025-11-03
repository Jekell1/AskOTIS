      * COPYMEMBER: LIBLP/LPWSLSJ
      *********************************************************
      *        LIBLP/LPWSLSJ: LSJFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LSJFILE
      *********************************************************
       01  LSJ-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LSJ-PATH                   PIC X(35)  VALUE SPACES.
       01  LSJ-ALL-PATH.
           03  LSJ-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES LSJ-ALL-BASE.
               05  LSJ-ALL-BASE-USR  PIC X(05).
               05  LSJ-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSJ-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSJ-ALL-DATA          PIC X(06).
           03  FILLER                PIC X(11)  VALUE "/LP/LSJFILE".
