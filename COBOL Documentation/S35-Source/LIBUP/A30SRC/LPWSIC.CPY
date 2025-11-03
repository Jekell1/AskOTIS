      * COPYMEMBER: LIBLP/LPWSIC
      *********************************************************
      *        LIBLP/LPWSIC: ICFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/ICFILE
      *********************************************************
       01  IC-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  IC-PATH                   PIC X(35)  VALUE SPACES.
       01  IC-ALL-PATH.
           03  IC-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES IC-ALL-BASE.
               05  IC-ALL-BASE-USR   PIC X(05).
               05  IC-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  IC-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  IC-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/ICFILE".
