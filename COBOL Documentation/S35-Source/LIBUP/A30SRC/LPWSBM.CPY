      * COPYMEMBER: LIBLP/LPWSBM
      *********************************************************
      *        LIBLP/LPWSBM: BMFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/BMFILE
      *********************************************************
       01  BM-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  BM-PATH                   PIC X(35)  VALUE SPACES.
       01  BM-ALL-PATH.
           03  BM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES BM-ALL-BASE.
               05  BM-ALL-BASE-USR   PIC X(05).
               05  BM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  BM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  BM-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/BMFILE".
