      * COPYMEMBER: LIBLP/LPWSML
      *********************************************************
      *        LIBLP/LPWSML: MLFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/MLFILE
      *********************************************************
       01  ML-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  ML-PATH                   PIC X(35)  VALUE SPACES.
       01  ML-ALL-PATH.
           03  ML-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES ML-ALL-BASE.
               05  ML-ALL-BASE-USR   PIC X(05).
               05  ML-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  ML-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  ML-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/MLFILE".
