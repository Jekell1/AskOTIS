      * COPYMEMBER: LIBGB/EOSPOOLW
      *=================================================================
      *    /USR/DATA/R#/SSBBBB/EO/XXX######X####XXXXXX
      *                              MMDDYY
      *=================================================================
       01  EO-SPOOL-OVERRIDE               PIC X(01)   VALUE SPACES.
       01  EO-SPOOL.
           03  EO-SPOOL-DIR.
               05  EO-SPOOL-BASE           PIC X(09).
               05  FILLER                  PIC X(01)   VALUE "/".
               05  EO-SPOOL-MACHINE        PIC X(02).
               05  FILLER                  PIC X(01)   VALUE "/".
               05  EO-SPOOL-DATA           PIC X(06).
               05  FILLER                  PIC X(04)   VALUE "/EO/".
               05  EO-SPOOL-PCODE          PIC X(03).
               05  EO-SPOOL-DATE           PIC 9(06).
               05  EO-SPOOL-DATE-NAME      REDEFINES EO-SPOOL-DATE
                                           PIC 9(06).
           03  EO-SPOOL-FILE.
               05  EO-SPOOL-SLASH          PIC X(01).
               05  EO-SPOOL-STEP           PIC 9(04).
               05  EO-SPOOL-NAME           PIC X(06).
               05  EO-SPOOL-SUF            PIC X(03).

       01  EO-ALL-SPOOL.
           03  EO-ALL-SPOOL-DIR.
               05  EO-ALL-SPOOL-BASE           PIC X(09).
               05  FILLER                      PIC X(01)   VALUE "/".
               05  EO-ALL-SPOOL-MACHINE        PIC X(02).
               05  FILLER                      PIC X(01)   VALUE "/".
               05  EO-ALL-SPOOL-DATA           PIC X(06).
               05  FILLER                      PIC X(04)   VALUE "/EO/".
               05  EO-ALL-SPOOL-PCODE          PIC X(03).
               05  EO-ALL-SPOOL-DATE           PIC 9(06).
               05  EO-ALL-SPOOL-DATE-NAME      REDEFINES
                   EO-ALL-SPOOL-DATE           PIC 9(06).
           03  EO-ALL-SPOOL-FILE.
               05  EO-ALL-SPOOL-SLASH          PIC X(01).
               05  EO-ALL-SPOOL-STEP           PIC 9(04).
               05  EO-ALL-SPOOL-NAME           PIC X(06).
               05  EO-ALL-SPOOL-SUF            PIC X(03).

