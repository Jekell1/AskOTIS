      * COPYMEMBER: LIBSP/SPWSTRWINFO
      ******************************************************************
      *    LIBSP/SPWSTRWINFO: TRWFILE PATH WORKERS
      *    /USR/DATA/R#/GB/TRWINFO
      * REV:
      * 050401 BAH MOVED FROM GLOBAL SP TO GLOBAL GB
      ******************************************************************
       01  TRW-INFO-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  TRW-INFO-PATH                   PIC X(40)  VALUE SPACES.
       01  FILLER                    REDEFINES TRW-INFO-PATH.
           03  TRW-INFO-PATH-BEG           PIC X(13).
           03  TRW-INFO-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES TRW-INFO-PATH-FILE.
               05  FILLER                  PIC X(03).
               05  TRW-INFO-PATH-SQL       PIC X(19).

       01  TRW-INFO-PATH-FORMAT.
           03  TRW-INFO-PATH-BASE          PIC X(09).
           03  FILLER            REDEFINES TRW-INFO-PATH-BASE.
               05  TRW-INFO-PATH-BASE-USR  PIC X(05).
               05  TRW-INFO-PATH-BASE-DIR  PIC X(04).
           03  FILLER                      PIC X(01)  VALUE "/".
           03  TRW-INFO-PATH-MACHINE       PIC X(02).
           03  FILLER                      PIC X(11)  VALUE
                                                      "/GB/TRWINFO".

