      * COPYMEMBER: LIBLP/LPWSBY
      *********************************************************
      *        LIBLP/LPWSBY: BYFILE PATH WORKERS
      *        /USR/DATA/XX/LP/BYFILE
      *********************************************************
       01  BY-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  BY-PATH                   PIC X(35)  VALUE SPACES.
       01  BY-ALL-PATH.
           03  BY-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES BY-ALL-BASE.
               05  BY-ALL-BASE-USR   PIC X(05).
               05  BY-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  BY-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/BYFILE".
