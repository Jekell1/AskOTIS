      * COPYMEMBER: LIBLP/LPFDTRD.CPY
      ******************************************************************
      *
      * DESCRIPTION:  DAILY TRANSACTION FILE [DEPOSIT]
      *
      * FILE INFO  :  PRIMARY LAYOUT IN LIBLP/LPFDTR.CPY
      *
      * TRD1-KEY = TD-BRNO, TD-DATE
      * TRD2-KEY = TD-DATE, TD-BRNO
      *
      *=================================================================
      * REV:
      * MJD 062795 ADDED TD-USERID, TD-POSTDATE, TP-POSTTIME
      * MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  CS 160913 CHANGED TD-POSTDATE 9(6) TO 9(8)                   
      *  CS 160915 ADDED TD-CHECK-DEPOSIT FOR SCANNED
      *            CHECKS, WORLD PR# 1188
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      * BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210426 REMOVED FUTURE FIELDS
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  TRD-SIZE                    VALUE 170.
       01  TRD-REC.
           03  TRD1-KEY.
               05  TD-BRNO             PIC 9(4).
      $XFD DATE=YYYYMMDD
               05  TD-DATE             PIC 9(8).
      *-------------------------------------------------
      $XFD DATE=YYYYMMDD
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

      $XFD DATE=YYYYMMDD
           03  TD-POSTDATE         PIC 9(8)     COMP-3.
           03  TD-POSTTIME         PIC 9(6)     COMP-3.
           03  FILLER              PIC X(101).
