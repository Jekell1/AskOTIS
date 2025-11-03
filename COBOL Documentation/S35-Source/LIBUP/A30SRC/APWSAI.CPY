      * COPYMEMBER: LIBAP/APWSAI
      ******************************************************************
      *    LIBAP/APWSAI: AIFILE PATH WORKERS
      *    /USR/DATA/R#/AP/AIFILE
      ******************************************************************
       01  AI-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  AI-PATH                   PIC X(35)   VALUE SPACES.
       01  AI-ALL-PATH.
           03  AI-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES AI-ALL-BASE.
               05  AI-ALL-BASE-USR   PIC X(05).
               05  AI-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AI-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/AP/AIFILE".
