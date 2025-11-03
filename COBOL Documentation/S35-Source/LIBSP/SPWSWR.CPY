      * COPYMEMBER: LIBSP/SPWSWR
      ******************************************************************
      *    LIBSP/SPWSWR: WRFILE PATH WORKERS
      *    /USR/DATA/R#/SP/WRFILE
      ******************************************************************
       01  WR-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  WR-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES WR-PATH.
           03  WR-PATH-BEG           PIC X(13).
           03  WR-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES WR-PATH-FILE.
               05  FILLER            PIC X(03).
               05  WR-PATH-SQL       PIC X(19).

       01  WR-PATH-FORMAT.
           03  WR-PATH-BASE          PIC X(09).
           03  FILLER                REDEFINES WR-PATH-BASE.
               05  WR-PATH-BASE-USR  PIC X(05).
               05  WR-PATH-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  WR-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/SP/WRFILE".

