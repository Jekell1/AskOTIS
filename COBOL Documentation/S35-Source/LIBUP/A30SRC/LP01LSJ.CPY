      * COPYMEMBER: LIBLP/LP01LSJ
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *       JUDGMENT INFORMATION
      *
      * REV:
      *  BAH 120330 ADDED "J" JUDGMENT INFORMATION CITIZENS PR# 3990
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      ******************************************************************
       78  LSJ-SIZE                       VALUE 750.
       01  LSJ-REC.
           03  LSJ1-KEY.
               05  LSJ-BRNO               PIC 9(4).
               05  LSJ-ACCTNO             PIC 9(8).
               05  LSJ-SEQNO              PIC 99.

      *>>XDD DATE=YYYYMMDD
           03  LSJ-LTOUCH-DATE            PIC 9(8)     COMP-3.

           03  LSJUDGMENT-AREA.
      *>>XDD DATE=YYYYMMDD
               05  LSJ-FILEDATE         PIC 9(8)  COMP-3.
               05  LSJ-CASE-NO          PIC X(15).
               05  LSJ-AMT              PIC S9(7)V99   COMP-3.
               05  LSJ-COUNTY           PIC X(25).
               05  LSJ-CLERK-NAME       PIC X(25).
               05  LSJ-CLERK-ADDR       PIC X(25).
               05  LSJ-CLERK-CITY       PIC X(15).
               05  LSJ-CLERK-STATE      PIC X(2).
               05  LSJ-CLERK-ZIP        PIC X(10).
               05  LSJ-CLERK-PHONE      PIC 9(10) COMP-3.

               05  LSJ-SHERIFF-FEE      PIC S9(7)V99   COMP-3.
               05  LSJ-COURT-FEE        PIC S9(7)V99   COMP-3.
               05  LSJ-ATTY-FEE         PIC S9(7)V99   COMP-3.

      *>>XDD DATE=YYYYMMDD
               05  LSJ-DISMISS-DATE     PIC 9(8)  COMP-3.
               05  LSJ-GARN-INFO        PIC X(25).
               05  LSJ-EMPLOY-NAME      PIC X(25).
               05  LSJ-EMPLOY-ADDR      PIC X(25).
               05  LSJ-EMPLOY-CITY      PIC X(15).
               05  LSJ-EMPLOY-STATE     PIC X(2).
               05  LSJ-EMPLOY-ZIP       PIC X(10).
      *>>XDD DATE=YYYYMMDD
               05  LSJ-GARN-DATE        PIC 9(8)  COMP-3.

               05  LSJ-JD-FILED-ON      PIC X.
               05  LSJ-GARN-FILED-ON    PIC X.
               05  LSJ-ATTY-NAME        PIC X(25).
               05  LSJ-ATTY-ADDR        PIC X(25).
               05  LSJ-ATTY-CITY        PIC X(15).
               05  LSJ-ATTY-STATE       PIC X(2).
               05  LSJ-ATTY-ZIP         PIC X(10).
               05  LSJ-ATTY-PHONE       PIC 9(10) COMP-3.

               05  FILLER                PIC X(386).

