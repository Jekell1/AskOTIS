      * COPYMEMBER: LIBLP/LPWSER
      ******************************************************************
      *    LIBLP/LPWSER: EO ERFILE PATH WORKERS
      *    /USR/DATA/R#/SSBBBB/EO/XXX######/0000LOG
      ******************************************************************
       01  ER-PATH-OVERRIDE            PIC X(01)     VALUE SPACES.
       01  ER-PATH                     PIC X(50).
       01  ER-ALL-PATH.
           03  ER-ALL-BASE             PIC X(09).
           03  FILLER                  REDEFINES ER-ALL-BASE.
               05  ER-ALL-BASE-USR     PIC X(05).
               05  ER-ALL-BASE-DIR     PIC X(04).
           03  FILLER                  PIC X(01)   VALUE "/".
           03  ER-ALL-MACHINE          PIC X(02).
           03  FILLER                  PIC X(01)   VALUE "/".
           03  ER-ALL-DATA             PIC X(06).
           03  FILLER                  PIC X(04)   VALUE "/EO/".
           03  ER-ALL-SPOOL-PCODE      PIC X(03).
           03  ER-SPOOL-PCODE          REDEFINES ER-ALL-SPOOL-PCODE
                                       PIC X(03).
           03  ER-ALL-SPOOL-DATE       PIC 9(06).
           03  ER-SPOOL-DATE           REDEFINES ER-ALL-SPOOL-DATE
                                       PIC 9(06).
           03  FILLER                  PIC X(08)     VALUE "/0000LOG".
