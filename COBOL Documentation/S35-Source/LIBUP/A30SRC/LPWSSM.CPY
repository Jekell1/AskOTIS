      * COPYMEMBER: LIBLP/LPWSSM
      *********************************************************
      *        LIBLP/LPWSSM: SMFILE PATH WORKERS
      *        /USR/DATA/XX/LP/SMFILE
      *
      *REV:
      *********************************************************
       01  SM-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  SM-PATH                   PIC X(35)  VALUE SPACES.
       01  SM-ALL-PATH.
           03  SM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES SM-ALL-BASE.
               05  SM-ALL-BASE-USR   PIC X(05).
               05  SM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  SM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/SMFILE".
