      * COPYMEMBER: LIBAP/APWSRI
      ******************************************************************
      *    LIBAP/APWSRI: RIFILE PATH WORKERS
      *    /USR/DATA/R#/AP/RIFILE
      ******************************************************************
       01  RI-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  RI-PATH                   PIC X(35)   VALUE SPACES.
       01  RI-ALL-PATH.
           03  RI-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES RI-ALL-BASE.
               05  RI-ALL-BASE-USR   PIC X(05).
               05  RI-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  RI-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/AP/RIFILE".
