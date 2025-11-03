      * COPYMEMBER: LIBLP/LPWSCI
      ******************************************************************
      * DESCRIPTION   :  CUSTOMER INFORMATION FILE
      *
      *    CI-PATH = /USR/DATA/XX/XX####/LP/CIFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      *=================================================================
      * REV:
      *  110131 BAH WORLD PR# 739
      *
      ******************************************************************
       01  CI-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  CI-PATH                   PIC X(50)  VALUE SPACES.
       01  FILLER                    REDEFINES CI-PATH.
           03  CI-PATH-BEG           PIC X(13).
           03  CI-PATH-OWNST         PIC X(02).
           03  CI-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  CI-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES CI-PATH-FILE.
               05  FILLER            PIC X(03).
               05  CI-PATH-SQL       PIC X(12).

       01  CI-ALL-PATH.
           03  CI-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES CI-ALL-BASE.
               05  CI-ALL-BASE-USR   PIC X(05).
               05  CI-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  CI-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  CI-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/CIFILE".
