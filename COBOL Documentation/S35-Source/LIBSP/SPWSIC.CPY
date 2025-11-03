      * COPYMEMBER: LIBSP/SPWSIC
      ******************************************************************
      *    IRS PAYEE TYPE "B" 1099-C EXTRACTION FILE
      *    LIBSP/SPWSIC: %%FILE PATH WORKERS
      *    /USR/DATA/R#/SP/ICFILE.####
      ******************************************************************
       01  IC-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  IC-PATH                   PIC X(40)   VALUE SPACES.
       01  IC-PATH-FORMAT.
           03  IC-PATH-BASE          PIC X(09).
           03  FILLER                REDEFINES IC-PATH-BASE.
               05  IC-PATH-BASE-USR  PIC X(05).
               05  IC-PATH-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  IC-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(28)   VALUE "/SP/ICFILE".
