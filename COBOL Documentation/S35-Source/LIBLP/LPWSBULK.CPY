      * COPYMEMBER: LIBLP/LPWSBULK
      *191009 ONLY CREATING 1 TAG IN GLOBAL DP
       01  BULK-PATH.
           03  BULK-DIR.
               05  BULK-ALL-BASE           PIC X(09).
               05  FILLER                  REDEFINES BULK-ALL-BASE.
                   07  BULK-ALL-BASE-USR   PIC X(05).
                   07  BULK-ALL-BASE-DIR   PIC X(04).
               05  FILLER                  PIC X(01)   VALUE "/".
               05  BULK-ALL-MACHINE        PIC XX.
      *        05  BULK-ALL-DATA           PIC X(6).
               05  FILLER                  PIC X(3)    VALUE "/DP".
           03  BULK-FILE.
               05  FILLER                  PIC X VALUE "/".
               05  BULK-FRBR               PIC 9(4).
               05  FILLER                  PIC X(4)    VALUE "BULK".
               05  BULK-TOBR               PIC 9(4).
      *01  TO-TAG-CREATE.
      *    03  TO-TAG-GREATER-THAN         PIC X       VALUE ">".
      *    03  TO-TAG-PATH.
      *        05  TO-TAG-ALL-BASE         PIC X(09).
      *        05  FILLER                  REDEFINES TO-TAG-ALL-BASE.
      *            07  TO-TAG-ALL-BASE-USR PIC X(05).
      *            07  TO-TAG-ALL-BASE-DIR PIC X(04).
      *        05  FILLER                  PIC X(01)   VALUE "/".
      *        05  TO-TAG-ALL-MACHINE      PIC XX.
      *        05  FILLER                  PIC X       VALUE "/".
      *        05  TO-TAG-ALL-DATA         PIC X(6).
      *        05  FILLER                  PIC X(4)    VALUE "/DP/".
      *        05  TO-TAG-FRBR             PIC 9(4).
      *        05  FILLER                  PIC X(7)    VALUE "BULKUPD".
      *        05  TO-TAG-TOBR             PIC 9(4).
       01  FROM-TAG-CREATE.
           03  FROM-TAG-GREATER-THAN       PIC X       VALUE ">".
           03  FROM-TAG-PATH.
               05  FROM-TAG-ALL-BASE       PIC X(09).
               05  FILLER                  REDEFINES FROM-TAG-ALL-BASE.
                   07  FROM-TAG-ALL-BASE-USR PIC X(05).
                   07  FROM-TAG-ALL-BASE-DIR PIC X(04).
               05  FILLER                  PIC X(01)   VALUE "/".
               05  FROM-TAG-ALL-MACHINE    PIC XX.
               05  FILLER                  PIC X       VALUE "/".
      *        05  FROM-TAG-ALL-DATA       PIC X(6).
               05  FILLER                  PIC X(3)    VALUE "DP/".
               05  FROM-TAG-FRBR           PIC 9(4).
               05  FILLER                  PIC X(7)    VALUE "BULKUPD".
               05  FROM-TAG-TOBR           PIC 9(4).
