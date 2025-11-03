      * COPYMEMBER: LIBSP/SPWSIM
      ******************************************************************
      *    IRS DISKETTE FILE
      *    LIBSP/SPWSIM: IMFILE PATH WORKERS
      *    /USR/DATA/R#/SP/IRSTAX.###
      ******************************************************************
       01  IM-PATH-OVERRIDE             PIC X(01)   VALUE SPACES.
       01  IM-PATH                      PIC X(35)   VALUE SPACES.
       01  IM-PATH-FORMAT.
           03  IM-PATH-BASE             PIC X(09).
           03  FILLER                   REDEFINES IM-PATH-BASE.
               05  IM-PATH-BASE-USR     PIC X(05).
               05  IM-PATH-BASE-DIR     PIC X(04).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  IM-PATH-MACHINE          PIC X(02).
           03  FILLER                   PIC X(10)   VALUE "/SP/IRSTAX".
           03  IM-PATH-SUFFIX.
               05  FILLER               PIC X(01)   VALUE ".".
               05  IM-PATH-FILE-COUNT   PIC 9(03)   VALUE 1.
