      * COPYMEMBER: LIBLP/LPWSTY
      *********************************************************
      *        LIBLP/LPWSTY: TYFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/TYFILE
      *********************************************************
       01  TY-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  TY-PATH                   PIC X(35)  VALUE SPACES.
       01  TY-ALL-PATH.
           03  TY-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TY-ALL-BASE.
               05  TY-ALL-BASE-USR   PIC X(05).
               05  TY-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  TY-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  TY-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/TYFILE".
