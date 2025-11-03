      * COPYMEMBER: LIBLP/LPWSEA
      *********************************************************
      *        LIBLP/LPWSEA: EAFILE PATH WORKERS
      *        /USR/DATA/XX/LP/EAFILE
      *********************************************************
       01  EA-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  EA-PATH                   PIC X(35)  VALUE SPACES.
       01  EA-ALL-PATH.
           03  EA-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES EA-ALL-BASE.
               05  EA-ALL-BASE-USR   PIC X(05).
               05  EA-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  EA-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/EAFILE".
