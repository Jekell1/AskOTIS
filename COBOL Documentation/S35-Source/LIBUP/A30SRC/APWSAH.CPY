      * COPYMEMBER: LIBAP/APWSAH
      *********************************************************
      *    LIBAP/APWSAH: AHFILE PATH WORKERS
      *    /USR/DATA/R#/AP/AHFILE
      ******************************************************************
       01  AH-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  AH-PATH                   PIC X(35)   VALUE SPACES.
       01  AH-ALL-PATH.
           03  AH-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES AH-ALL-BASE.
               05  AH-ALL-BASE-USR   PIC X(05).
               05  AH-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AH-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/AP/AHFILE".
