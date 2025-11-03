      * COPYMEMBER: LIBSP/SPWSIL
      ******************************************************************
      *    IRS 1098 LOAN EXTRACTION FILE
      *    LIBSP/SPWSIL: ILFILE PATH WORKERS
      *    /USR/DATA/R#/SP/ILFILE
      ******************************************************************
       01  IL-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  IL-PATH                   PIC X(40)   VALUE SPACES.
       01  IL-PATH-FORMAT.
           03  IL-PATH-BASE          PIC X(09).
           03  FILLER                REDEFINES IL-PATH-BASE.
               05  IL-PATH-BASE-USR  PIC X(05).
               05  IL-PATH-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  IL-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/SP/ILFILE".
