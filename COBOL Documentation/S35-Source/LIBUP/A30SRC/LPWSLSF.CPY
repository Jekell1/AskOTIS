      * COPYMEMBER: LIBLP/LPWSLSF
      *********************************************************
      *        LIBLP/LPWSLSF: LSFFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LSFFILE
      *********************************************************
       01  LSF-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LSF-PATH                   PIC X(35)  VALUE SPACES.
       01  LSF-ALL-PATH.
           03  LSF-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES LSF-ALL-BASE.
               05  LSF-ALL-BASE-USR  PIC X(05).
               05  LSF-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSF-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSF-ALL-DATA          PIC X(06).
           03  FILLER                PIC X(11)  VALUE "/LP/LSFFILE".
