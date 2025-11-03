      * COPYMEMBER: LIBLP/LPFDTRW.CPY
      ******************************************************************
      *
      * DESCRIPTION:  DAILY TRANSACTION FILE [CASH BALANCING RECORD]
      *
      * FILE INFO  :  PRIMARY LAYOUT IN LIBLP/LPFDTR.CPY
      *               TR1-CODE = W - CASH BALANCING RECORD
      *               LOCAL ED/TR###  (####=MMYY)              RSZ=950
      *
      *=================================================================
      * REV:
      *  CS 160916 CHANGED TW-TILLNO FROM PIC X TO 9(2)
      *  CS 160916 CHANGED TW-CLOSE-TABLE FROM OCCURS 20 TO 35
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      ******************************************************************
       01  TW-REC.
           03  TW-TRW-KEY.
               05  TW-DAY              PIC 99.
               05  TW-BRNO             PIC 9(4).
               05  TW-CODE             PIC X.
               05  TW-TILLNO           PIC 9(2).
               05  TW-FILLER           PIC X(18).
      *-------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  TW-LTOUCH-DATE          PIC 9(8) COMP-3.
           03  TW-CLOSE-TABLE      OCCURS 20.
               05  TW-POSTNAME     PIC X(08).
               05  TW-TIME         PIC 9(04)     COMP-3.
               05  TW-AMOUNT       PIC S9(07)V99 COMP-3.
               05  TW-OVERSHORT    PIC S9(07)V99 COMP-3.
           03  FILLER              PIC X(498).
