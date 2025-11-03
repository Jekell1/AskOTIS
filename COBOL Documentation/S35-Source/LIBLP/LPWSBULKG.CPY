      * COPYMEMBER: LIBLP/LPWSBULKG
      *=================================================================
      * A15: /USR/DATA/R1/SS####/ED/XXXXBULK####
      *                             -A--    -B--
      *      A = GROUP NAME
      *      B = TO BRANCH
      *
      * A32: /USR/DATA/R1/DP/XXXXBULK####
      *                      -A--    -B--
      *      A = GROUP NAME
      *      B = TO BRANCH
      *
      *=================================================================
       01  BULK-PATH.
           03  BULK-DIR.
               05  BULK-ALL-BASE             PIC X(09).
               05  FILLER                    REDEFINES BULK-ALL-BASE.
                   07  BULK-ALL-BASE-USR     PIC X(05).
                   07  BULK-ALL-BASE-DIR     PIC X(04).
               05  FILLER                    PIC X(01)   VALUE "/".
               05  BULK-ALL-MACHINE          PIC XX.
               05  FILLER                    PIC X(3)    VALUE "/DP".
           03  BULK-PATH-FILENAME.
               05  FILLER                    PIC X       VALUE "/".
               05  BULK-FROM-TO              PIC X(15).

      *=================================================================
      * A15: >/USR/DATA/R1/SS####/ED/XXXXBULKUPD####
      *                              -A--       -B--
      *      A = GROUP NAME
      *      B = TO BRANCH
      *
      * A32: >/USR/DATA/R1/DP/XXXXBULKUPD####
      *                       -A--       -B--
      *      A = GROUP NAME
      *      B = TO BRANCH
      *
      *=================================================================
       01  TO-TAG-CREATE.
           03  TO-TAG-GREATER-THAN           PIC X       VALUE ">".
           03  TO-TAG-PATH.
               05  TO-TAG-ALL-BASE           PIC X(09).
               05  FILLER                    REDEFINES TO-TAG-ALL-BASE.
                   07  TO-TAG-ALL-BASE-USR   PIC X(05).
                   07  TO-TAG-ALL-BASE-DIR   PIC X(04).
               05  FILLER                    PIC X(01)   VALUE "/".
               05  TO-TAG-ALL-MACHINE        PIC XX.
               05  FILLER                    PIC X(4)    VALUE "/DP/".
               05  TO-TAG-FROM-TO            PIC X(15).

      *=================================================================
      * A15: >/USR/DATA/R1/SS####/ED/XXXXBULKUPD####
      *                              -A--       -B--
      *      A = GROUP NAME
      *      B = TO BRANCH
      *
      * A32: >/USR/DATA/R1/DP/####XXXXBULKUPD####
      *                       -A-|-B-|       -C--
      *      A = FROM BRANCH (GENERATE FROM BRANCH-GROUP LOGIC)
      *      B = GROUP NAME
      *      C = TO BRANCH
      *=================================================================
       01  FROM-TAG-CREATE.
           03  FROM-TAG-GREATER-THAN         PIC X       VALUE ">".
           03  FROM-TAG-PATH.
               05  FROM-TAG-ALL-BASE         PIC X(09).
               05  FILLER          REDEFINES FROM-TAG-ALL-BASE.
                   07  FROM-TAG-ALL-BASE-USR PIC X(05).
                   07  FROM-TAG-ALL-BASE-DIR PIC X(04).
               05  FILLER                    PIC X(01)   VALUE "/".
               05  FROM-TAG-ALL-MACHINE      PIC XX.
               05  FILLER                    PIC X(4)    VALUE "/DP/".
               05  FROM-TAG-FROM-TO          PIC X(20).
