      * COPYMEMBER: LIBLP/LPWSCI
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPWSCI
      *                  /USR/DATA/XX/XX####/LP/CIFILE
      *
      * DESCRIPTION   :  CUSTOMER INFORMATION FILE
      *
      *=================================================================
      * REV:
      *  110131 BAH WORLD PR# 739
      *
      ******************************************************************
       01  CI-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  CI-PATH                   PIC X(50)  VALUE SPACES.
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
