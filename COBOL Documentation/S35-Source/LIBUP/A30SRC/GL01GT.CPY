      * COPYMEMBER: LIBGL/GL01GT
      ******************************************************************
      *
      * GENERAL LEDGER TRANSACTION FILE
      *
      *         PRIMARY KEY:  GT-SOURCE
      *                       GT-DATE2
      *                       GT-BRANCH
      *                       GT-ACCTNO 
      *                       GT-DATE3
      *                       GT-DRCRFG
      *                       GT-LINE
      *
      *    SECONDARY KEY (2): GT-DATE1
      *
      *    SECONDARY KEY (3): GT-BRANCH
      *                       GT-ACCTNO 
      *                       GT-DATE3
      *                       GT-DRCRFG
      *                       GT-LINE
      *
      *  NOTE: GT-DATE1 GT-DATE2 GT-DATE3 ARE ACTIVE CCYYMMDD DATES.
      *        IN A15, GT-DATE1 GT-DATE2 GT-DATE3 ARE YYMMDD DATES.
      *
      * NOTE:  IF YOU CHANGE THIS RECORD, YOU MUST MAKE THE CHANGE TO
      *        THE BRANCH CONSOLIDATION GENERAL TRANSACTION FILE 
      *        (LIBBC/BC01GT): GT-REC IS BCGT-REC.
      *
      *=================================================================
      * REV:
      * BLV 080797 ADDED GT-SMITH-SENT (WORLD)
      * MJD 090597 EXPANDED GT-AMT TO BE S9(9)V99
      * MJD 990126 ----- UPDATED IN FLEXGEN -----
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * BLF 070119 ADDED GT-POST-USERID, GT-POST-DATE, GT-POST-TIME,
      *            SUPPORT PR# 2780
      *
      * KEC 2015.1223 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         INCREASE GT-REC; GT-SIZE = 175
      *         ADDED GT-LTOUCH-DATE.
      *         CHANGED DATES TO CCYYMMDD FORMAT; PIC 9(8) COMP-3.
      *         CHANGED COMP-6 TO BE COMP-3.
      *         REMOVE GT3-KEY (A15) AND MADE GT4-KEY (A15)
      *         TO NOW BE THE NEW GT3-KEY (A30).
      *         INCREASED GT-POST-USERID FROM 8 TO 10 BYTES.
      *
      ******************************************************************
       78  GT-SIZE                             VALUE 175.
       01  GT-REC.
           03  GT-FLAG                         PIC X.
           03  GT-REF                          PIC X(14).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GT-DATE1 GT-DATE2 GT-DATE3 ARE ACTIVE CCYYMMDD AS OF A30
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  GT2-KEY.
      *>>XDD DATE=YYYYMMDD
               05  GT-DATE1                    PIC 9(8).
           03  GT1-KEY.
               05  GT-SOURCE                   PIC 9.
      *>>XDD DATE=YYYYMMDD
               05  GT-DATE2                    PIC 9(8).
               05  GT3-KEY.
                   07  GT-BRANCH               PIC 9(4).
                   07  GT-ACCTNO               PIC 9(8).
      *>>XDD DATE=YYYYMMDD
                   07  GT-DATE3                PIC 9(8).
                   07  GT-DRCRFG               PIC X.
                   07  GT-LINE                 PIC 9(6).

      *>>XDD DATE=YYYYMMDD
           03  GT-LTOUCH-DATE                  PIC 9(08)      COMP-3.
           03  GT-AMT                          PIC S9(9)V99   COMP-3.
           03  GT-EXPL                         PIC X(32).
           03  GT-PERIOD                       PIC 99         COMP-3.
           03  GT-SMITH-SENT                   PIC X.
           03  GT-POST-USERID                  PIC X(10).
      *>>XDD DATE=YYYYMMDD
           03  GT-POST-DATE                    PIC 9(8)       COMP-3.
           03  GT-POST-TIME                    PIC 9(4)       COMP-3.
           03  FILLER                          PIC X(52).
