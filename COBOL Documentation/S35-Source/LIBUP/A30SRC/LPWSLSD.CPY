      * COPYMEMBER: LIBLP/LPWSLSD
      *********************************************************
      *        LIBLP/LPWSLSD: LSDFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LSDFILE
      *********************************************************
       01  LSD-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LSD-PATH                   PIC X(35)  VALUE SPACES.
       01  LSD-ALL-PATH.
           03  LSD-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES LSD-ALL-BASE.
               05  LSD-ALL-BASE-USR  PIC X(05).
               05  LSD-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSD-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSD-ALL-DATA          PIC X(06).
           03  FILLER                PIC X(11)  VALUE "/LP/LSDFILE".
