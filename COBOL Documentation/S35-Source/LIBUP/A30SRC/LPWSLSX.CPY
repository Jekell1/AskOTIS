      * COPYMEMBER: LIBLP/LPWSLSX
      *********************************************************
      *        LIBLP/LPWSLSX: LSXFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LSXFILE
      *********************************************************
       01  LSX-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LSX-PATH                   PIC X(35)  VALUE SPACES.
       01  LSX-ALL-PATH.
           03  LSX-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES LSX-ALL-BASE.
               05  LSX-ALL-BASE-USR  PIC X(05).
               05  LSX-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSX-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSX-ALL-DATA          PIC X(06).
           03  FILLER                PIC X(11)  VALUE "/LP/LSXFILE".
