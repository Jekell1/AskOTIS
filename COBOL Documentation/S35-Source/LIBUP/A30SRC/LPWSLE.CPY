      * COPYMEMBER: LIBLP/LPWSLE
      *********************************************************
      *        LIBLP/LPWSLE: LEFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LEFILE
      *********************************************************
       01  LE-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LE-PATH                   PIC X(35)  VALUE SPACES.
       01  LE-ALL-PATH.
           03  LE-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LE-ALL-BASE.
               05  LE-ALL-BASE-USR   PIC X(05).
               05  LE-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LE-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LE-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/LEFILE".
