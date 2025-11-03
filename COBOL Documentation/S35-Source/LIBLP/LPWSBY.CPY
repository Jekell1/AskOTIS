      * COPYMEMBER: LIBLP/LPWSBY
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE WORKERS - A THRU B
      *
      * BYA-PATH = /USR/DATA/R#/LP/BYAFILE
      *            |----BEG----||-----FILE----|
      *                            |---SQL----|
      *
      * BYB-PATH = /USR/DATA/R#/LP/BYBFILE
      *            |----BEG----||-----FILE----|
      *                            |---SQL----|
      *
      *=================================================================
      * REV:
      * JKC 2023-0606 COMMENTED OUT THE SPLIT BYFILE INTO BYAFILE AND
      *         BYBFILE; COPIED IN THE A30 VERSION FOR FULL FILE.
      *
      ******************************************************************
       01  BY-PATH-OVERRIDE            PIC X(01)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  BYA-PATH                    PIC X(35)  VALUE SPACES.
       01  FILLER                      REDEFINES BYA-PATH.
           03  BYA-PATH-BEG            PIC X(13).
           03  BYA-PATH-FILE           PIC X(22).
           03  FILLER                  REDEFINES BYA-PATH-FILE.
               05  FILLER              PIC X(3).
               05  BYA-PATH-SQL        PIC X(19).

       01  BYA-ALL-PATH.
           03  BYA-ALL-BASE            PIC X(09).
           03  FILLER                  REDEFINES BYA-ALL-BASE.
               05  BYA-ALL-BASE-USR    PIC X(05).
               05  BYA-ALL-BASE-DIR    PIC X(04).
           03  FILLER                  PIC X(01)  VALUE "/".
           03  BYA-ALL-MACHINE         PIC X(02).
           03  FILLER                  PIC X(23)  VALUE "/LP/BYAFILE".

      *-----------------------------------------------------------------
       01  BYB-PATH                    PIC X(35)  VALUE SPACES.
       01  FILLER REDEFINES BYB-PATH.
           03  BYB-PATH-BEG            PIC X(13).
           03  BYB-PATH-FILE           PIC X(22).
           03  FILLER                  REDEFINES BYB-PATH-FILE.
               05  FILLER              PIC X(3).
               05  BYB-PATH-SQL        PIC X(19).

       01  BYB-ALL-PATH.
           03  BYB-ALL-BASE            PIC X(09).
           03  FILLER                  REDEFINES BYB-ALL-BASE.
               05  BYB-ALL-BASE-USR    PIC X(05).
               05  BYB-ALL-BASE-DIR    PIC X(04).
           03  FILLER                  PIC X(01)  VALUE "/".
           03  BYB-ALL-MACHINE         PIC X(02).
           03  FILLER                  PIC X(23)  VALUE "/LP/BYBFILE".

      ******************************************************************
      *01  BY-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
      *01  BY-PATH                   PIC X(35)  VALUE SPACES.
      *01  BY-ALL-PATH.
      *    03  BY-ALL-BASE           PIC X(09).
      *    03  FILLER                REDEFINES BY-ALL-BASE.
      *        05  BY-ALL-BASE-USR   PIC X(05).
      *        05  BY-ALL-BASE-DIR   PIC X(04).
      *    03  FILLER                PIC X(01)  VALUE "/".
      *    03  BY-ALL-MACHINE        PIC X(02).
      *    03  FILLER                PIC X(23)  VALUE "/LP/BYFILE".

