      * COPYMEMBER: LIBLP/LPFDTRD
      **************************************************
      *               TR-CODE  =  "D"
      *                  DEPOSIT
      *
      * REV:
      *  MJD 062795 ADDED TD-USERID, TD-POSTDATE, TP-POSTTIME
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  BLM 160915 ADDED TD-CHECK-DEPOSIT FOR SCANNED
      *             CHECKS, WORLD PR# 1188
      **************************************************
       01  TD-REC.
           03  TD-DAY              PIC 99      COMP.
           03  TD-BRNO             PIC 9(4)    COMP.
           03  TD-CODE             PIC X.
           03  TD-FILLER2          PIC X(10).
      *-------------------------------------------------
           03  TD-DEPOSIT          PIC S9(7)V99 COMP-3.
           03  TD-BANK             PIC 9(11)    COMP-3.
           03  TD-CASH             PIC 9(11)    COMP-3.
           03  TD-GLFG             PIC X.
           03  TD-GLOVERSHORT      PIC 9(11)    COMP-3.
           03  TD-OVERSHORT        PIC S9(5)V99 COMP-3.
           03  TD-USERID           PIC X(10).
           03  TD-POSTDATE         PIC 9(6)     COMP-3.
           03  TD-POSTTIME         PIC 9(6)     COMP-3.
           03  TD-CHECK-DEPOSIT    PIC S9(7)V99 COMP-3.
           03  FILLER              PIC X(485).
