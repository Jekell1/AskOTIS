      * COPYMEMBER: LIBLP/LPWSLTA
      *********************************************************
      *        LIBLP/LPWSLTA: LIENHOLDER PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LTAFILE
      *********************************************************
       01  LTA-PATH-OVERRIDE   PIC X(01)  VALUE SPACES.
       01  LTA-PATH            PIC X(35)  VALUE SPACES.
       01  LTA-ALL-PATH.
           03  LTA-ALL-BASE           PIC X(09).
           03  FILLER REDEFINES LTA-ALL-BASE.
               05  LTA-ALL-BASE-USR   PIC X(05).
               05  LTA-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTA-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTA-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)  VALUE "/LP/LTAFILE".
