      * COPYMEMBER: LIBLP/LP01LSJ
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *       JUDGMENT INFORMATION
      *
      *    LSJ1-KEY = LSJ-BRNO, LSJ-ACCTNO, LSJ-SEQNO
      *
      * REV:
      *  BAH 120330 ADDED "J" JUDGMENT INFORMATION CITIZENS PR# 3990
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *   CS 190506 ADD FUTURE FIELDS
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      *  BAH 20230600 ADDED FILLER
      ******************************************************************
       78  LSJ-SIZE                       VALUE 670.
       01  LSJ-REC.
           03  LSJ1-KEY.
               05  LSJ-BRNO         PIC 9(4).
               05  LSJ-ACCTNO       PIC 9(8).
               05  LSJ-SEQNO        PIC 99.

      $XFD DATE=YYYYMMDD
           03  LSJ-LTOUCH-DATE      PIC 9(8)       COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSJ-FILEDATE         PIC 9(8)       COMP-3.
           03  LSJ-CASE-NO          PIC X(15).
           03  LSJ-AMT              PIC S9(7)V99   COMP-3.
           03  LSJ-COUNTY           PIC X(25).
           03  LSJ-CLERK-NAME       PIC X(25).
           03  LSJ-CLERK-ADDR       PIC X(25).
           03  LSJ-CLERK-CITY       PIC X(15).
           03  LSJ-CLERK-STATE      PIC X(2).
           03  LSJ-CLERK-ZIP        PIC X(10).
           03  LSJ-CLERK-PHONE      PIC 9(10)      COMP-3.

           03  LSJ-SHERIFF-FEE      PIC S9(7)V99   COMP-3.
           03  LSJ-COURT-FEE        PIC S9(7)V99   COMP-3.
           03  LSJ-ATTY-FEE         PIC S9(7)V99   COMP-3.

      $XFD DATE=YYYYMMDD
           03  LSJ-DISMISS-DATE     PIC 9(8)       COMP-3.
           03  LSJ-GARN-INFO        PIC X(25).
           03  LSJ-EMPLOY-NAME      PIC X(25).
           03  LSJ-EMPLOY-ADDR      PIC X(25).
           03  LSJ-EMPLOY-CITY      PIC X(15).
           03  LSJ-EMPLOY-STATE     PIC X(2).
           03  LSJ-EMPLOY-ZIP       PIC X(10).
      $XFD DATE=YYYYMMDD
           03  LSJ-GARN-DATE        PIC 9(8)     COMP-3.

           03  LSJ-JD-FILED-ON      PIC X.
           03  LSJ-GARN-FILED-ON    PIC X.
           03  LSJ-ATTY-NAME        PIC X(25).
           03  LSJ-ATTY-ADDR        PIC X(25).
           03  LSJ-ATTY-CITY        PIC X(15).
           03  LSJ-ATTY-STATE       PIC X(2).
           03  LSJ-ATTY-ZIP         PIC X(10).
           03  LSJ-ATTY-PHONE       PIC 9(10)    COMP-3.

           03  FILLER                 PIC X(306).
