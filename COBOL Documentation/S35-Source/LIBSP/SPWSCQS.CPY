      * COPYMEMBER: LIBSP/SPWSCQS
      * 190214  BREAK OUT THE HEADER RECORD IN EXISTING CQFILE, ITS NOW A
      * SUMMARY (CQS)
      *********************************************************
      *        LIBSP/SPWSCQ: CQFILE PATH WORKERS
      *********************************************************
       01  CQS-PATH-OVERRIDE    PIC X(01) VALUE SPACES.
       01  CQS-PATH             PIC X(35) VALUE SPACES.
       01  FILLER                    REDEFINES CQS-PATH.
           03  CQS-PATH-BEG           PIC X(13).
           03  CQS-PATH-OWNST         PIC X(02).
           03  CQS-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  CQS-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES CQS-PATH-FILE.
               05  FILLER            PIC X(03).
               05  CQS-PATH-SQL       PIC X(12).

       01  CQS-ALL-PATH.
           03  CQS-ALL-BASE     PIC X(09).
           03  FILLER                REDEFINES CQS-ALL-BASE.
               05  CQS-ALL-BASE-USR   PIC X(05).
               05  CQS-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CQS-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CQS-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(11)   VALUE "/SP/CQSFILE".

