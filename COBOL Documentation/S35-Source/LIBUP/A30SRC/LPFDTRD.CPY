      * COPYMEMBER: LIBLP/LPFDTRD.CPY
      ******************************************************************
      *
      * DESCRIPTION:  DAILY TRANSACTION FILE [DEPOSIT]
      *
      * FILE INFO  :  PRIMARY LAYOUT IN LIBLP/LPFDTR.CPY
      *               TR1-CODE = D - DEPOSIT
      *               LOCAL ED/TR###  (####=MMYY)              RSZ=950
      *
      *=================================================================
      * REV:
      * MJD 062795 ADDED TD-USERID, TD-POSTDATE, TP-POSTTIME
      * MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  CS 160913 CHANGED TD-POSTDATE 9(6) TO 9(8)                   
      *  CS 160915 ADDED TD-CHECK-DEPOSIT FOR SCANNED
      *            CHECKS, WORLD PR# 1188
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      ******************************************************************
       01  TD-REC.
           03  TD-TRD-KEY.
               05  TD-DAY              PIC 99.                
               05  TD-BRNO             PIC 9(4).
               05  TD-CODE             PIC X.
               05  TD-FILLER           PIC X(20).
      *-------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  TD-LTOUCH-DATE      PIC 9(8)  COMP-3.
           03  TD-DEPOSIT          PIC S9(7)V99 COMP-3.
           03  TD-CHECK-DEPOSIT    PIC S9(7)V99 COMP-3.
      *%GL NUMBER
           03  TD-BANK             PIC 9(11)    COMP-3.
      *%GL NUMBER
           03  TD-CASH             PIC 9(11)    COMP-3.
           03  TD-GLFG             PIC X.
      *%GL NUMBER
           03  TD-GLOVERSHORT      PIC 9(11)    COMP-3.

           03  TD-OVERSHORT        PIC S9(5)V99 COMP-3.
           03  TD-USERID           PIC X(10).

      *>>XDD DATE=YYYYMMDD
           03  TD-POSTDATE         PIC 9(8)     COMP-3.
           03  TD-POSTTIME         PIC 9(6)     COMP-3.
           03  FILLER              PIC X(866).
