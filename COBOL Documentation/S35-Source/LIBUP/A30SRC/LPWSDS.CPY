      * COPYMEMBER: LIBLP/LPWSDS
      *********************************************************
      *        LIBLP/LPWSDS: DSFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/DSFILE
      *********************************************************
       01  DS-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  DS-PATH                   PIC X(35)  VALUE SPACES.
       01  DS-ALL-PATH.
           03  DS-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES DS-ALL-BASE.
               05  DS-ALL-BASE-USR   PIC X(05).
               05  DS-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DS-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DS-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/DSFILE".
