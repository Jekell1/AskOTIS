      * COPYMEMBER: LIBSP/SPWSBH
      ******************************************************************
      *    LIBLP/LPWSBH: BHFILE PATH WORKERS
      *    /USR/DATA/R#/LP/BHFILE
      ******************************************************************
       78  BH-PATH-DEFAULT-DIR                  VALUE "../../LP".
       78  BH-PATH-DEFAULT                      VALUE "../../LP/BHFILE".
       01  BH-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  BH-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES BH-PATH.
           03  BH-PATH-BEG           PIC X(13).
           03  BH-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES BH-PATH-FILE.
               05  FILLER            PIC X(03).
               05  BH-PATH-SQL       PIC X(19).

       01  BH-PATH-FORMAT.
           03  BH-PATH-BASE          PIC X(09).
           03  FILLER                REDEFINES BH-PATH-BASE.
               05  BH-PATH-BASE-USR  PIC X(05).
               05  BH-PATH-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  BH-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/BHFILE".
