      * COPYMEMBER: LIBSP/SPWSCP
      ******************************************************************
      *
      * LIBSP/SPWSCP: CPFILE PATH WORKERS
      *
      *=================================================================
      * BEFORE A32/A34, THE CPFILE HAD A GLOBAL AND LOCAL VERSION.
      * SINCE  A32/A34, THE CPFILE MERGED GLOBAL INTO THE LOCAL WHICH
      * THE USE OF CP-PATH-OVERRIDE AND CP-BRNO DETERMINES IF IT IS
      * GLOBAL OR LOCAL IN ITS USAGE.
      *
      * GLOBAL VERSION:
      *    - CP-PATH-OVERRIDE = "G"
      *    - CP-BRNO = ZEROES
      *             
      *                     CP-PATH = /USR/DATA/R#/SP/CPFILE
      *                               |----BEG----||-----FILE----|
      *                                               |---SQL----|
      *             
      * LOCAL VERSION:
      *    - CP-PATH-OVERRIDE = "B", "Y" OR SPACES
      *    - CP-BRNO NOT = ZEROES
      *             
      *                 CP-PATH = /USR/DATA/R#/XX####/SP/CPFILE
      *                           |----BEG----||-MID-||-----FILE----|
      *             
      *=================================================================
      * REV:
      * BLV 112800 ADDED CP-GLOBAL-PATH; IF USING GLOBAL COLLECTOR
      *            QUEUING, EXTRACTION MAINTENANCES & RUNS MAY USE
      *            GLOBAL COLLECTOR POOL FILE IF BRANCH HAS GLOBAL
      *            QUEUING TURNED ON - THE ACTUAL FILE LAYOUT & USAGE
      *            WILL BE IDENTICAL, FILE IS JUST IN A DIFFERENT SPOT
      *
      * KEC 2018-1026 FOR A32 & A34, THE CPFILE IS BOTH GLOBAL & LOCAL.
      *               SO CP-PATH IS REDEFINED FOR LOCAL THEN GLOBAL.
      *               CP-PATH-SQL LAYOUT WORKS FOR GLOBAL & LOCAL.
      *               BECAUSE THERE WILL BE A LOCAL & GLOBAL VERSIONS
      *               OF THE CPFILE WITH BOTH BEING IN THE SAME LOCATION
      *               NOW, I CHANGED THE FILE NAME:
      *                  GLOBAL = SP/GCPFILE
      *                  LOCAL  = SP/CPFILE
      *               ADD 88 LEVELS TO CP-PATH-OVERRIDE SINCE ADDING
      *               NEW VALUE OF "G" FOR KNOWING TO USE THE
      *               GLOBAL FILE (GCPFILE) OR LOCAL (CPFILE).
      *
      * KEC 2019-0405 WE ARE MERGING THE GLOBAL & LOCAL FILES INTO ONE
      *               FILE.  THE GLOBAL VERSION WILL USE "G" IN
      *               CP-PATH-OVERRIDE AND ZEROES IN THE BRANCH NUMBER
      *               WITHIN THE PRIMARY KEY.
      *
      ******************************************************************
       01  CP-PATH-OVERRIDE            PIC X(01)     VALUE SPACES.
           88  CP-PATH-OVERRIDE-EXTERNAL             VALUE " ".
           88  CP-PATH-OVERRIDE-BRANCH               VALUE "B".
           88  CP-PATH-OVERRIDE-GLOBAL               VALUE "G".
           88  CP-PATH-OVERRIDE-YES                  VALUE "Y".

       01  CP-PATH                     PIC X(35)     VALUE SPACES.
       01  FILLER                      REDEFINES CP-PATH.
           03  CP-PATH-BEG             PIC X(13).
           03  CP-PATH-OWNST           PIC X(02).
           03  CP-PATH-OWNBR           PIC 9(04).
           03  FILLER                  PIC X(01).
           03  CP-PATH-FILE            PIC X(15).
           03  FILLER                  REDEFINES CP-PATH-FILE.
               05  FILLER              PIC X(03).
               05  CP-PATH-SQL         PIC X(12).
       01  FILLER                      REDEFINES CP-PATH.
           03  CP-GLOBAL-PATH-BEG      PIC X(13).
           03  CP-GLOBAL-PATH-FILE     PIC X(22).
           03  FILLER                  REDEFINES CP-GLOBAL-PATH-FILE.
               05  FILLER              PIC X(03).
               05  CP-GLOBAL-PATH-SQL  PIC X(19).

       01  CP-ALL-PATH.
           03  CP-ALL-BASE             PIC X(09).
           03  FILLER                  REDEFINES CP-ALL-BASE.
               05  CP-ALL-BASE-USR     PIC X(05).
               05  CP-ALL-BASE-DIR     PIC X(04).
           03  FILLER                  PIC X(01)     VALUE "/".
           03  CP-ALL-MACHINE          PIC X(02).
           03  FILLER                  PIC X(01)     VALUE "/".
           03  CP-ALL-DATA             PIC X(06).
           03  FILLER                  PIC X(10)     VALUE "/SP/CPFILE".

       01  CP-GLOBAL-PATH.
           03  CP-GLOBAL-BASE          PIC X(09).
           03  FILLER                  PIC X(01)     VALUE "/".
           03  CP-GLOBAL-MACHINE       PIC X(02).
           03  FILLER                  PIC X(10)     VALUE "/SP/CPFILE".

