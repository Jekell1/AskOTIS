      * COPYMEMBER: LIBLP/LPWSDT
      *********************************************************
      *        LIBLP/LPWSDT: DTFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/DTFILE
      *********************************************************
       01  DT-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  DT-PATH                   PIC X(35)  VALUE SPACES.
       01  DT-ALL-PATH.
           03  DT-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES DT-ALL-BASE.
               05  DT-ALL-BASE-USR   PIC X(05).
               05  DT-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DT-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DT-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/DTFILE".
