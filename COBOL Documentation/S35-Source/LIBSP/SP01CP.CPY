      * COPYMEMBER: LIBSP/SP01CP
      ******************************************************************
      *
      * DESCRIPTION   : COLLECTOR POOL FILE
      *
      *  CP1-KEY = CP-BRNO, CP-ID
      *
      *=================================================================
      * GLOBAL CPFILE | CP-BRNO = ZEROES
      * LOCAL  CPFILE | CP-BRNO > ZEROES
      *
      *=================================================================
      * REV:
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * BAH 160406 CHANGED FOR A30
      *
      * KEC 2016.1006 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         MOVED CP-LTOUCH-DATE TO BE AFTER CP1-KEY.
      *
      * KEC 2018.1026 MOVED CP-BRNO WITHIN CP1-KEY
      * BAH 2019.0215 REMOVED CP-MEMBERS X(240); USING
      *               CP-COL-ID PIC X(3) OCCURS 80 AS PRIMARY.
      *  CS 190529 REMOVE ENDING FILLER
      * BAH 20230612 ADDED FILLER
      ******************************************************************
       78  CP-SIZE                      VALUE 370.
       01  CP-REC.
           03  CP1-KEY.
               05  CP-BRNO              PIC 9(4).  
               05  CP-ID                PIC X(3).

      $XFD DATE=YYYYMMDD
           03  CP-LTOUCH-DATE           PIC 9(8) COMP-3.
           03  CP-DESC                  PIC X(20).

           03  CP-COL-ID                PIC X(3) OCCURS 80.

           03  CP-ALPHA-FLAG            PIC X.
      *  COLLECTOR-COUNT & LAST-SUBSCRIPT ARE WORKERS IN CPRUN
      *  USED IN SPLITING ACCOUNTS EVENLY THROUGHOUT POOL
           03  CP-TOTAL-ACCOUNTS        PIC 9(7) COMP-3.
           03  CP-COLLECTOR-COUNT       PIC 9(3).
           03  CP-LAST-SUBSCRIPT        PIC 9(3).
           03  FILLER                   PIC X(87).
