      * COPYMEMBER: LIBUT/UTWSZM
      *********************************************************
      *        LIBUT/UTWSZM: ZMFILE PATH WORKERS - SQL SERVER
      *********************************************************
       01  ZM-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  ZM-PATH                   PIC X(35)  VALUE SPACES.
*G2G   01  FILLER                    REDEFINES ZM-PATH.
*G2G       03  ZM-PATH-BEG           PIC X(13).
*G2G       03  ZM-PATH-FILE          PIC X(22).
*G2G       03  FILLER                REDEFINES ZM-PATH-FILE.
*G2G           05  FILLER            PIC X(03).
*G2G           05  ZM-PATH-SQL       PIC X(19).

       01  ZM-ALL-PATH.
           03  ZM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES ZM-ALL-BASE.
               05  ZM-ALL-BASE-USR   PIC X(05).
               05  ZM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  ZM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/UT/ZMFILE".
