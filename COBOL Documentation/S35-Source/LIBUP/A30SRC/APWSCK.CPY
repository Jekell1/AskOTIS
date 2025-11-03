      * COPYMEMBER: LIBAP/APWSCK
      ******************************************************************
      *    LIBAP/APWSCK: CKFILE PATH WORKERS
      *    /USR/DATA/R#/AP/CKFILE
      ******************************************************************
       01  CK-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  CK-PATH                   PIC X(35)   VALUE SPACES.
       01  CK-ALL-PATH.
           03  CK-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES CK-ALL-BASE.
               05  CK-ALL-BASE-USR   PIC X(05).
               05  CK-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CK-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/AP/CKFILE".
