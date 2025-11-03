      * COPYMEMBER: LIBAP/APWSTM
      ******************************************************************
      *    LIBAP/APWSTM: TMFILE PATH WORKERS
      *    /USR/DATA/R#/AP/TMFILE
      ******************************************************************
       01  TM-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  TM-PATH                   PIC X(35)   VALUE SPACES.
       01  TM-ALL-PATH.
           03  TM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TM-ALL-BASE.
               05  TM-ALL-BASE-USR   PIC X(05).
               05  TM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  TM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/AP/TMFILE".
