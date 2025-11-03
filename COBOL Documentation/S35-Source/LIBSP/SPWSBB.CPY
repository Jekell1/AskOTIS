      * COPYMEMBER: LIBSP/SPWSBB
      ******************************************************************
      *    LIBLP/LPWSBB: BBFILE PATH WORKERS
      *    /USR/DATA/R#/LP/BBFILE
      ******************************************************************
       78  BB-PATH-DEFAULT-DIR                  VALUE "../../LP".
       78  BB-PATH-DEFAULT                      VALUE "../../LP/BBFILE".
       01  BB-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  BB-PATH                   PIC X(40)  VALUE SPACES.
       01  FILLER                    REDEFINES BB-PATH.
           03  BB-PATH-BEG           PIC X(13).
           03  BB-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES BB-PATH-FILE.
               05  FILLER            PIC X(03).
               05  BB-PATH-SQL       PIC X(19).

       01  BB-PATH-FORMAT.
           03  BB-PATH-BASE          PIC X(09).
           03  FILLER                REDEFINES BB-PATH-BASE.
               05  BB-PATH-BASE-USR  PIC X(05).
               05  BB-PATH-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  BB-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/BBFILE".
