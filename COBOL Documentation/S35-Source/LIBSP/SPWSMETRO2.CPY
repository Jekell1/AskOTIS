      * COPYMEMBER: LIBSP/SPWSMETRO2
      ******************************************************************
      *
      *  LIBSP/SPWSMETRO2:   TRWFILE PATH WORKERS
      *
      ******************************************************************
       01  TRW-PATH.
           03  TRW-PATH-BASE             PIC X(09).
           03  FILLER                    PIC X(01)   VALUE "/".
           03  TRW-PATH-MACHINE          PIC X(02).
           03  FILLER                    PIC X(04)   VALUE "/ED/".
           03  TRW-PATH-FILENAME         PIC X(24)   VALUE "METRO2".
           03  FILLER                    REDEFINES TRW-PATH-FILENAME.
               05  TRW-PATH-WORLD-FIXED  PIC X(06).
               05  TRW-PATH-WORLD-DOT    PIC X(01).
               05  TRW-PATH-WORLD-GROUP  PIC X(04).
               05  TRW-PATH-WORLD-BRNO   REDEFINES TRW-PATH-WORLD-GROUP
                                         PIC 9(04).
               05  FILLER                PIC X(13).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * FOR MULITPLE INFO ID'S
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  TRW-PATH-2.
           03  TRW-PATH-2-BASE           PIC X(09).
           03  FILLER                    PIC X(01)   VALUE "/".
           03  TRW-PATH-2-MACHINE        PIC X(02).
           03  FILLER                    PIC X(04)   VALUE "/ED/".
           03  TRW-PATH-2-FILENAME       PIC X(06)  VALUE "METRO2".
           03  FILLER                    PIC X(01)  VALUE ".".
           03  TRW-PATH-ID               PIC 9(01)  VALUE ZEROES.
           03  FILLER                    PIC X(16).

      *=================================================================
       01  TRW-E-PATH.
           03  TRW-E-PATH-BASE            PIC X(09).
           03  FILLER                     PIC X(01)   VALUE "/".
           03  TRW-E-PATH-MACHINE         PIC X(02).
           03  FILLER                     PIC X(04)   VALUE "/ED/".
           03  TRW-E-PATH-FILENAME        PIC X(24)   VALUE "METRO2.E".
           03  FILLER                     REDEFINES TRW-E-PATH-FILENAME.
               05  TRW-E-PATH-FIXED       PIC X(08).
               05  TRW-E-PATH-PERIOD      PIC X(01).
               05  TRW-E-DATE.
                   07  TRW-E-PATH-YY      PIC 9(02).
                   07  TRW-E-PATH-MM      PIC 9(02).
                   07  TRW-E-PATH-DD      PIC 9(02).
               05  FILLER                 PIC X(09).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * FOR MULITPLE INFO ID'S
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  TRW-E2-PATH.
           03  TRW-E2-PATH-BASE          PIC X(09).
           03  FILLER                    PIC X(01)   VALUE "/".
           03  TRW-E2-PATH-MACHINE       PIC X(02).
           03  FILLER                    PIC X(04)   VALUE "/ED/".
           03  TRW-E2-PATH-FILENAME      PIC X(08)  VALUE "METRO2.E".
           03  FILLER                    PIC X(01)  VALUE ".".
           03  TRW-E2-PATH-ID            PIC 9(01)  VALUE ZEROES.
           03  FILLER                    PIC X(14).

       01  TRW-PATH-WORLD-INTERIM.
           03  TRW-WORLD-I-PATH-BASE     PIC X(09).
           03  FILLER                    PIC X(01)  VALUE "/".
           03  TRW-WORLD-I-PATH-MACHINE  PIC X(02).
           03  FILLER                    PIC X(11)  VALUE "/ED/METRO2.".
           03  TRW-WORLD-I-DATE.
               05  TRW-WORLD-I-PATH-YR   PIC 9(02).
               05  TRW-WORLD-I-PATH-MO   PIC 9(02).
               05  TRW-WORLD-I-PATH-DA   PIC 9(02).
           03  FILLER                    PIC X(11).

