      * COPYMEMBER: LIBLP/LPWSLX
      *********************************************************
      *        LIBLP/LPWSLX: LXFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/GB/LXFILE
      *********************************************************
       01  LX-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LX-PATH                   PIC X(35)  VALUE SPACES.
       01  LX-ALL-PATH.
           03  LX-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LX-ALL-BASE.
               05  LX-ALL-BASE-USR   PIC X(05).
               05  LX-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LX-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LX-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/GB/LXFILE".
