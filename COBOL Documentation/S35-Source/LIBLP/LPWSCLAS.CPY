      * COPYMEMBER: LIBLP/LPWSCLAS
      ******************************************************************
      *    /USR/DATA/R#/SSBBBB/ED/##CLASSTFR##
      ******************************************************************
       01  CLAS-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  CLAS-PATH                   PIC X(50)   VALUE SPACES.
       01  CLAS-PATH-FORMAT.
           03  CLAS-PATH-BASE          PIC X(09).
           03  FILLER                  REDEFINES CLAS-PATH-BASE.
               05  CLAS-PATH-BASE-USR  PIC X(05).
               05  CLAS-PATH-BASE-DIR  PIC X(04).
           03  FILLER                  PIC X(01)   VALUE "/".
           03  CLAS-PATH-MACHINE       PIC X(02).
           03  FILLER                  PIC X(01)   VALUE "/".
           03  CLAS-PATH-DATA          PIC X(06).
           03  FILLER                  PIC X(04)   VALUE "/ED/".
           03  CLAS-PATH-FRLC          PIC 9(02).
           03  CLAS-FRLC               REDEFINES CLAS-PATH-FRLC
                                       PIC 9(02).
           03  FILLER                  PIC X(08)   VALUE "CLASSTFR".
           03  CLAS-PATH-TOLC          PIC 9(02).
           03  CLAS-TOLC               REDEFINES CLAS-PATH-TOLC
                                       PIC 9(02).
