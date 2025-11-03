      * COPYMEMBER: LIBLP/LPWSDM
      *********************************************************
      *        LIBLP/LPWSDM: DMFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/DMFILE
      *********************************************************
       01  DM-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  DM-PATH                   PIC X(35)  VALUE SPACES.
       01  DM-ALL-PATH.
           03  DM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES DM-ALL-BASE.
               05  DM-ALL-BASE-USR   PIC X(05).
               05  DM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DM-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/DMFILE".
