      * COPYMEMBER: LIBLP/LPWSPC
      *********************************************************
      *        LIBLP/LPWSPC: PCFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/PCFILE
      *********************************************************
       01  PC-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  PC-PATH                   PIC X(35)  VALUE SPACES.
       01  PC-ALL-PATH.
           03  PC-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES PC-ALL-BASE.
               05  PC-ALL-BASE-USR   PIC X(05).
               05  PC-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  PC-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  PC-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/PCFILE".
