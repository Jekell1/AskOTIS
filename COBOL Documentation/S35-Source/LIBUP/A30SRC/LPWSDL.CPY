      * COPYMEMBER: LIBLP/LPWSDL
      *********************************************************
      *    LIBLP/LPWSDL: DLFILE PATH WORKERS
      *    /USR/DATA/XX/XX####/LP/DLFILE
      ******************************************************************
       01  DL-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  DL-PATH                   PIC X(35)   VALUE SPACES.
       01  DL-ALL-PATH.
           03  DL-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES DL-ALL-BASE.
               05  DL-ALL-BASE-USR   PIC X(05).
               05  DL-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  DL-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  DL-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/LP/DLFILE".
