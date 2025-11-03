      * COPYMEMBER: LIBLP/LPWSUD
      *********************************************************
      *        LIBLP/LPWSUD: UDFILE PATH WORKERS
      *        /USR/DATA/XX/LP/UDFILE
      *********************************************************
       01  UD-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  UD-PATH                   PIC X(35)  VALUE SPACES.
       01  UD-ALL-PATH.
           03  UD-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES UD-ALL-BASE.
               05  UD-ALL-BASE-USR   PIC X(05).
               05  UD-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  UD-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(10)  VALUE "/LP/UDFILE".
