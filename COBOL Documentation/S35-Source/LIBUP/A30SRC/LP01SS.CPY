      * COPYMEMBER: LIBLP/LP01SS
      *************************************************************
      *                     'SS'
      *            ( CREDIT SCORE STATISTICS FILE )
      *
      *          ONLY CONTRACTUAL
      *          MARK NEEDS TO DETERMINE SCORE BREAKS, & HOW MANY BREAKS
      *          REMAINING???
      *
      * REV:
      *  160526 BAH EXPANDED
      *************************************************************
       78  SS-SIZE                        VALUE 1240.
       01  SS-REC.
           03  SS1-KEY.
               05  SS-DATE-YYYYMM         PIC 9(6).
               05  SS-BRNO                PIC 9(4).
               05  SS-CLASS               PIC 9(3).
      *>>XDD DATE=YYYYMMDD
           03  SS-LTOUCH-DATE             PIC 9(8) COMP-3.
           03  SS-DATA.
      *    -------------------------------------------------
      *    SS-NOW-BMO-LME
      *        1- 0 OR 1
      *        2- 2-488
      *        3- 489-534
      *        4- 535-562
      *        5- 563-591
      *        6- 592-612
      *        7- 613-639
      *        8- 640 AND OVER
      *
      *    SS-SCORE-TYPE
      *        1- CURRENT:  MOST RECENT SCORE, FROM BX-CREDIT-SCORE(1)
      *        2- ORIGINAL SCORE: FROM BX-BNKRPT-SCORE(1)
      *
      *    SS-DEL-BUCKET
      *        1- CUR
      *        2- POTS
      *        3- 30
      *        4- 60
      *        5- 90+
      *    -------------------------------------------------
               05  SS-SCORE                OCCURS 8.
                   07  SS-CUR-ORIG-SCORE   OCCURS 2.
                       09  SS-DEL-BUCKET   OCCURS 5.
                           11  SS-UNITS    PIC S9(7)    COMP-3.
                           11  SS-OUTBAL   PIC S9(9)V99 COMP-3.
                       09  SS-MTD-YTD-PL   OCCURS 2.
                           11  SS-NO-PL    PIC S9(7)    COMP-3.
                           11  SS-AMT-PL   PIC S9(9)V99 COMP-3.
      *    -------------------------------------------------

               05  FILLER                         PIC X(102).
