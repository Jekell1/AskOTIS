      * COPYMEMBER: LIBLP/LPWSLM
      *********************************************************
      *        LIBLP/LPWSLM: LMFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LMFILE
      *********************************************************
       01  LM-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LM-PATH                   PIC X(35)  VALUE SPACES.
       01  LM-ALL-PATH.
           03  LM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LM-ALL-BASE.
               05  LM-ALL-BASE-USR   PIC X(05).
               05  LM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LM-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/LMFILE".
