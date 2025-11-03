      * COPYMEMBER: LIBLP/LP01LSL
      ******************************************************************
      *    LOAN STATUS INFORMATION FILE : 
      *
      *        LEGAL INFORMATION
      *
      * REV:
      *  BAH 110215 ADDED "L" LEGAL INFORMATION FOR UNITED PR# 3707
      *  BAH 110803 ADDED LSL-DATE-CLOSED, LSL-REASON-CLOSED, UNITED PR# 3847
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      ******************************************************************
       78  LSL-SIZE                       VALUE 850.
       01  LSL-REC.
           03  LSL1-KEY.
               05  LSL-BRNO               PIC 9(4).
               05  LSL-ACCTNO             PIC 9(8).
               05  LSL-SEQNO              PIC 99.

      *>>XDD DATE=YYYYMMDD
           03  LSL-LTOUCH-DATE               PIC 9(8)     COMP-3.

           03  LS-LEGAL-AREA.
      *>>XDD DATE=YYYYMMDD
               05  LSL-DATE-SUIT-AUTH        PIC 9(8) COMP-3.
               05  LSL-DATE-SUIT-AUTH-BY     PIC X(20).
               05  LSL-ATTORNEY-NET-ASSIGN   PIC X(20).
               05  LSL-LOCAL-ATTORNEY        PIC X(20).
               05  LSL-ATTORNEY-ADDRESS      PIC X(60).
               05  LSL-ATTORNEY-PHONE        PIC 9(10) COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-DATE-ASSIGNED         PIC 9(8)  COMP-3.
               05  LSL-ATTORNEY-ACCTNO       PIC X(20).
               05  LSL-PRIN-BAL-SENT         PIC S9(7)V99 COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-RECEIVED-ACKNOW-DATE       PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-ATTORNEY-LETTER-SENT-DATE  PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-HOLD-DATE                  PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-RESPONSE-RECEIVED-DATE     PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-SUIT-DATE             PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-SUIT-FILED-DATE       PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-SERVICE-DATE          PIC 9(8)  COMP-3.
               05  LSL-CUSTOMER-SERVED       PIC X.
      *>>XDD DATE=YYYYMMDD
               05  LSL-ANSWER-FILED-DATE     PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-COURT-DATE            PIC 9(8)  COMP-3.
               05  LSL-JUDGMENT-TYPE         PIC X(20).
      *>>XDD DATE=YYYYMMDD
               05  LSL-JUDGMENT-OBTAINED-DATE PIC 9(8)  COMP-3.
               05  LSL-JUDGMENT-COPY-FILED   PIC X.
               05  LSL-JUDGMENT-AMT          PIC S9(7)V99 COMP-3.
               05  LSL-JUDGMENT-PRIN         PIC S9(7)V99 COMP-3.
               05  LSL-POST-JUDG-INT-RATE    PIC S9(3)V99 COMP-3.
               05  LSL-JUDG-INT-AMT          PIC S9(7)V99 COMP-3.
               05  LSL-ATTORNEY-FEES-ALLOWED PIC S9(7)V99 COMP-3.
               05  LSL-OTHER-FEES-ALLOWED    PIC S9(7)V99 COMP-3.
               05  LSL-JUDG-INT-RATE         PIC S9(3)V99 COMP-3.
               05  LSL-ASSET-USED-IN-SUIT    PIC X(20).
      *>>XDD DATE=YYYYMMDD
               05  LSL-GARN-FILED-DATE       PIC 9(8)  COMP-3.
               05  LSL-GARN-TERM-DAYS        PIC 9(3) COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-INTERROGATORY-SENT-DATE PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-INTERROGATORY-ANSW-DATE PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-FIFA-SENT-DATE          PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-FIFA-ANSWERED-DATE      PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-DATE-TO-RECV-PAY      PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-FOLLOWUP-DATE         PIC 9(8)  COMP-3.
      *>>XDD DATE=YYYYMMDD
               05  LSL-FIFA-RENEWED-DATE     PIC 9(8)  COMP-3.
               05  LSL-PAYS-RECV-ATTORNEY    PIC X.
      *>>XDD DATE=YYYYMMDD
               05  LSL-DATE-CLOSED           PIC 9(8)  COMP-3.
               05  LSL-REASON-CLOSED         PIC X(20).

               05  FILLER                     PIC X(479).
