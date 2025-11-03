      * COPYMEMBER: LIBLP/LP01LSL
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *        LEGAL INFORMATION
      *
      *    LSL1-KEY = LSL-BRNO, LSL-ACCTNO, LSL-SEQNO
      *
      * REV:
      *  BAH 110215 ADDED "L" LEGAL INFORMATION FOR UNITED PR# 3707
      *  BAH 110803 ADDED LSL-DATE-CLOSED, LSL-REASON-CLOSED, UNITED PR# 3847
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *   CS 190506 ADD FUTURE FIELDS
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * AMM 20210429 REMOVED FUTURE FIELDS, RESIZED FILE
      *  BAH 20230600 ADDED FILLER
      ******************************************************************
       78  LSL-SIZE                       VALUE 780.
       01  LSL-REC.
           03  LSL1-KEY.
               05  LSL-BRNO              PIC 9(4).
               05  LSL-ACCTNO            PIC 9(8).
               05  LSL-SEQNO             PIC 99.

      $XFD DATE=YYYYMMDD
           03  LSL-LTOUCH-DATE           PIC 9(8)     COMP-3.

      $XFD DATE=YYYYMMDD
           03  LSL-DATE-SUIT-AUTH        PIC 9(8)     COMP-3.
           03  LSL-DATE-SUIT-AUTH-BY     PIC X(20).
           03  LSL-ATTORNEY-NET-ASSIGN   PIC X(20).
           03  LSL-LOCAL-ATTORNEY        PIC X(20).
           03  LSL-ATTORNEY-ADDRESS      PIC X(60).
           03  LSL-ATTORNEY-PHONE        PIC 9(10)    COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-DATE-ASSIGNED         PIC 9(8)     COMP-3.
           03  LSL-ATTORNEY-ACCTNO       PIC X(20).
           03  LSL-PRIN-BAL-SENT         PIC S9(7)V99 COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-RECEIVED-ACKNOW-DATE  PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-ATTORNEY-LETTER-SENT-DATE  PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-HOLD-DATE             PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-RESPONSE-RECEIVED-DATE PIC 9(8)    COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-SUIT-DATE             PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-SUIT-FILED-DATE       PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-SERVICE-DATE          PIC 9(8)     COMP-3.
           03  LSL-CUSTOMER-SERVED       PIC X.
      $XFD DATE=YYYYMMDD
           03  LSL-ANSWER-FILED-DATE     PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-COURT-DATE            PIC 9(8)     COMP-3.
           03  LSL-JUDGMENT-TYPE         PIC X(20).
      $XFD DATE=YYYYMMDD
           03  LSL-JUDGMENT-OBTAINED-DATE PIC 9(8)    COMP-3.
           03  LSL-JUDGMENT-COPY-FILED   PIC X.
           03  LSL-JUDGMENT-AMT          PIC S9(7)V99 COMP-3.
           03  LSL-JUDGMENT-PRIN         PIC S9(7)V99 COMP-3.
           03  LSL-POST-JUDG-INT-RATE    PIC S9(3)V99 COMP-3.
           03  LSL-JUDG-INT-AMT          PIC S9(7)V99 COMP-3.
           03  LSL-ATTORNEY-FEES-ALLOWED PIC S9(7)V99 COMP-3.
           03  LSL-OTHER-FEES-ALLOWED    PIC S9(7)V99 COMP-3.
           03  LSL-JUDG-INT-RATE         PIC S9(3)V99 COMP-3.
           03  LSL-ASSET-USED-IN-SUIT    PIC X(20).
      $XFD DATE=YYYYMMDD
           03  LSL-GARN-FILED-DATE       PIC 9(8)     COMP-3.
           03  LSL-GARN-TERM-DAYS        PIC 9(3)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-INTERROGATORY-SENT-DATE PIC 9(8)   COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-INTERROGATORY-ANSW-DATE PIC 9(8)   COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-FIFA-SENT-DATE        PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-FIFA-ANSWERED-DATE    PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-DATE-TO-RECV-PAY      PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-FOLLOWUP-DATE         PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  LSL-FIFA-RENEWED-DATE     PIC 9(8)     COMP-3.
           03  LSL-PAYS-RECV-ATTORNEY    PIC X.
      $XFD DATE=YYYYMMDD
           03  LSL-DATE-CLOSED           PIC 9(8)     COMP-3.
           03  LSL-REASON-CLOSED         PIC X(20).

           03  FILLER                 PIC X(409).
