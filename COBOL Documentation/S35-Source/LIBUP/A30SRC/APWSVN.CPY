      * COPYMEMBER: LIBAP/APWSVN
      ******************************************************************
      *    LIBAP/APWSVN: VNFILE PATH WORKERS
      *    /USR/DATA/R#/AP/VNFILE
      ******************************************************************
       01  VN-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  VN-PATH                   PIC X(35)   VALUE SPACES.
       01  VN-ALL-PATH.
           03  VN-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES VN-ALL-BASE.
               05  VN-ALL-BASE-USR   PIC X(05).
               05  VN-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  VN-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/AP/VNFILE".
