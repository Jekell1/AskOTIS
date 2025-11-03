      * COPYMEMBER: LIBLP/LPWSSS
      *********************************************************
      *        LIBLP/LPWSSS: SSFILE PATH WORKERS
      *        /USR/DATA/XX/LP/SSFILE
      *********************************************************
       01  SS-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  SS-PATH                   PIC X(35)  VALUE SPACES.
       01  SS-ALL-PATH.
           03  SS-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES SS-ALL-BASE.
               05  SS-ALL-BASE-USR   PIC X(05).
               05  SS-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  SS-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/SSFILE".
