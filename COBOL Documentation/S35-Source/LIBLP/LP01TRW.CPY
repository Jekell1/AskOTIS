      * COPYMEMBER: LIBLP/LPFDTRW.CPY
      ******************************************************************
      *
      * DESCRIPTION:  TRANSACTION FILE [CASH BALANCING RECORD]
      *
      * FILE INFO  :  CASH BALANCING
      *               LP/TRWFILE
      *
      * TRW1-KEY = TW-BRNO, TW-DATE, TW-TILLNO
      * TRW2-KEY = TW-DATE, TW-BRNO, TW-TILLNO
      *
      *=================================================================
      * REV:
      *  CS 160916 CHANGED TW-TILLNO FROM PIC X TO 9(2)
      *  CS 160916 CHANGED TW-CLOSE-TABLE FROM OCCURS 20 TO 35
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      * BAH 20200325 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20210426 REMOVED FUTURE FIELDS
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  TRW-SIZE                    VALUE 650.
       01  TRW-REC.
           03  TRW1-KEY.
               05  TW-BRNO             PIC 9(4).
      $XFD DATE=YYYYMMDD
               05  TW-DATE             PIC 9(8).
               05  TW-TILLNO           PIC 9(2).
      *-------------------------------------------------
      $XFD DATE=YYYYMMDD
           03  TW-LTOUCH-DATE          PIC 9(8)      COMP-3.
           03  TW-CLOSE-TABLE      OCCURS 20.
               05  TW-POSTNAME         PIC X(08).
               05  TW-TIME             PIC 9(04)     COMP-3.
               05  TW-AMOUNT           PIC S9(07)V99 COMP-3.
               05  TW-OVERSHORT        PIC S9(07)V99 COMP-3.
           03  FILLER                  PIC X(211).
