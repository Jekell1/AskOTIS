      * COPYMEMBER: LIBLP/LPWSES
      *********************************************************
      *        LIBLP/LPWSES: ESCROW FILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/ESFILE
      *********************************************************
       01  ES-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  ES-PATH                   PIC X(35)  VALUE SPACES.
       01  ES-ALL-PATH.
           03  ES-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES ES-ALL-BASE.
               05  ES-ALL-BASE-USR   PIC X(05).
               05  ES-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  ES-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  ES-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/ESFILE".
