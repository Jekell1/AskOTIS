      * COPYMEMBER: LIBSP/SPWSGQS
      * 190214  BREAK OUT THE GLOBAL HEADER RECORD IN EXISTING GQFILE,
      *         SUMMARY
      *********************************************************
      *        LIBSP/SPWSGQ: GQFILE PATH WORKERS
      *********************************************************
       01  GQS-PATH-OVERRIDE    PIC X(01) VALUE SPACES.
       01  GQS-PATH             PIC X(35) VALUE SPACES.
       01  FILLER                    REDEFINES GQS-PATH.
           03  GQS-PATH-BEG           PIC X(13).
           03  GQS-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES GQS-PATH-FILE.
               05  FILLER            PIC X(03).
               05  GQS-PATH-SQL       PIC X(19).

       01  GQS-ALL-PATH.
           03  GQS-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES GQS-ALL-BASE.
               05  GQS-ALL-BASE-USR   PIC X(05).
               05  GQS-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GQS-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/SP/GQSFILE".
