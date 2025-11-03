      * COPYMEMBER: LIBLP/LPWSLH
      *********************************************************
      *        LIBLP/LPWSLH: LHFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LHFILE
      *********************************************************
       01  LH-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LH-PATH                   PIC X(35)  VALUE SPACES.
       01  LH-ALL-PATH.
           03  LH-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LH-ALL-BASE.
               05  LH-ALL-BASE-USR   PIC X(05).
               05  LH-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LH-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LH-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/LHFILE".
