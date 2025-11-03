      * COPYMEMBER: LIBLP/LPWSPD
      *********************************************************
      *        LIBLP/LPWSPD: PDFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/PDFILE
      *********************************************************
       01  PD-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  PD-PATH                   PIC X(35)  VALUE SPACES.
       01  PD-ALL-PATH.
           03  PD-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES PD-ALL-BASE.
               05  PD-ALL-BASE-USR   PIC X(05).
               05  PD-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  PD-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  PD-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/PDFILE".
