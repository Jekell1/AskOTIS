      * COPYMEMBER: LIBGL/GL01JD
      ******************************************************************
      *
      *  GENERAL LEDGER JOURNAL DETAIL LINE
      *  KEY 1 - DATE/ID/SEQ/LINE
      *
      *=================================================================
      *
      *  *NOTE*   *NOTE*  *NOTE*   *NOTE*  *NOTE*   *NOTE*  *NOTE*
      *
      *  WJ-REC [LIBGL/GLFDWJ] CONTAINS AN EXACT COPY OF
      *  JD-REC WITHIN IT WHICH IS STORED IN WJ-JD-REC.
      *  NEED TO UPDATE WJ-JD-REC IF UPDATING JD-REC.
      *
      *=================================================================
      * REV:
      * GLF 022498 INCREASE JD-ACCTNO FROM 10 TO 11 AND
      *            JD-BRANCH FROM 3 TO 4.
      * GLB 990616 ADD JD-GROUP-FLAG
      * GLB 991124 ADD JD-PCT, NEED AMOUNT AND PERCENTAGE
      * BLV 020318 INCREASE JD-LINE FROM 3 TO 4, INCREASE JD-AMT FROM
      *           9(7) TO 9(9), INCREASE UNUSED, PUT IN 01 INSTEAD OF FD
      * BLV 020415 REMOVE JD-PCT, NOT SURE WHAT IT'S FOR, NOT UPDATED,
      *            ENDS UP BEING 2020
      * BLV 020422 ADD JD-DRCR-FG, WILL INDICATE IF ENTRY IS A DEBIT OR
      *            A CREDIT
      * GLB 030730  ADD SEQUENCE TO JD1-KEY
      * GLB 030730  MODIFY JD-DATE FROM 6 TO 8 DIGITS YYYYMMDD
      * BLV 031009  ADD JD-ORIG-LINE
      * BLV 031010  ADD JD-PARENT-GROUP, INCREASED UNUSED
      *
      * KEC 2015.1214 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         INCREASE JD-REC; JD-SIZE = 150
      *         ADDED JD-LTOUCH-DATE.
      *         CHANGED COMP-6 TO BE COMP-3.
      *
      ******************************************************************
       78  JD-SIZE                             VALUE 150.
       01  JD-REC.
           03  JD1-KEY.
      *- - - - - -{ ACTIVE CCYYMMDD IN A15 }
      *>>XDD DATE=YYYYMMDD
               05  JD-DATE                     PIC 9(8).
               05  JD-ID                       PIC X(8).
               05  JD-SEQ                      PIC 9(2).
               05  JD-LINE                     PIC 9(4).

           03  JD-LTOUCH-DATE                  PIC 9(08)      COMP-3.
           03  JD-GROUP-FG                     PIC X.

           03  JD-NUMKEY                       PIC 9(11).
           03  JD-ALPKEY             REDEFINES JD-NUMKEY.
               05  JD-BRANCH                   PIC 9(4).
               05  JD-GROUP          REDEFINES JD-BRANCH
                                               PIC X(4).
               05  JD-ACCTNO                   PIC 9(7).

           03  JD-OFFSET-GROUP-FG              PIC X.

           03  JD-OFFSET-NUMKEY                PIC 9(11).
           03  JD-OFFSET-ALPKEY      REDEFINES JD-OFFSET-NUMKEY.
               05  JD-OFFSET-BRANCH            PIC 9(4).
               05  JD-OFFSET-GROUP   REDEFINES JD-OFFSET-BRANCH
                                               PIC X(4).
               05  JD-OFFSET-ACCTNO            PIC 9(7).

           03  JD-AMT                          PIC S9(9)V99.
           03  JD-EXPL                         PIC X(32).
           03  JD-DRCR-FG                      PIC X.
           03  JD-STATUS                       PIC X.
           03  JD-ORIG-LINE                    PIC 9(4)       COMP-3.
           03  JD-PARENT-GROUP                 PIC X(4).
           03  FILLER                          PIC X(47).
