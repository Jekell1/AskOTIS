      * COPYMEMBER: LIBSP/SPWSAQS
      ******************************************************************
      *    LIBSP/SPWSAQS: AQSFILE PATH WORKERS
      *
      * AQS-PATH = /USR/DATA/R#/SSBBBB/%%/%%%FILE
      *
      *
      ******************************************************************
       01  AQS-PATH-OVERRIDE         PIC X(01)   VALUE SPACES.
       01  AQS-PATH                  PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES AQS-PATH.
           03  AQS-PATH-BEG          PIC X(13).
           03  AQS-PATH-OWNST        PIC X(02).
           03  AQS-PATH-OWNBR        PIC 9(04).
           03  FILLER                PIC X(01).
           03  AQS-PATH-FILE         PIC X(15).
           03  FILLER                REDEFINES AQS-PATH-FILE.
               05  FILLER            PIC X(03).
               05  AQS-PATH-SQL      PIC X(12).

       01  AQS-ALL-PATH.
           03  AQS-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES AQS-ALL-BASE.
               05  AQS-ALL-BASE-USR  PIC X(05).
               05  AQS-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AQS-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AQS-ALL-DATA          PIC X(06).
           03  FILLER                PIC X(11)   VALUE "/SP/AQSFILE".
