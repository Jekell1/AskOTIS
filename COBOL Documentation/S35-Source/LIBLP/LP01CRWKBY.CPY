      * COPYMEMBER: LIBLP/LP01CRWKBY
      *************************************************************
      *                     'CRWKBY'
      *            (  WORKER BRANCH STATISTICS FILE )
      *                 N E W  VERSION
      *
      *       CRWKBY-DATA (1) = DIRECT LOAN SUM
      *                   (2) = SALES FINANCE - SERVICE ACCT SUM.
      *                   (3) = SERVICE ACCOUNT SUM.
      * REV:
      * 09/06/2016 BAH CHANGED DATE TO 9(6) AND CLASS 9(3)
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      *************************************************************
       01  CRWKBY-REC.
           03  CRWKBY-KEY.
               05  CRWKBY-DATE-CCYYMM         PIC 9(6).
               05  CRWKBY-BRNO                PIC 9(4).
               05  CRWKBY-CLASS               PIC 9(3).
           03  CRWKBY-DATA        OCCURS 3 TIMES.
               07  CRWKBY-NO                  PIC S9(8).
               07  CRWKBY-AMT                 PIC S9(13)V99.
