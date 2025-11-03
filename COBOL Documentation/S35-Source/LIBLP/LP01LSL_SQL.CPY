      * COPYMEMBER: LIBLP/LP01LSL_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN STATUS -  LEGAL
      *
      *=================================================================
      * REV:
      * AMM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LSL1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LSL1-CURSOR-STAT-GOOD             VALUE "00".
           88  LSL1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LSL1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLSL1-WHERE-BEG-KEY.
           03  QLSL1-WBEG-BRNO             PIC 9(04).
           03  QLSL1-WBEG-ACCTNO           PIC 9(08).
           03  QLSL1-WBEG-SEQNO            PIC 9(02).

       01  QLSL1-WHERE-END-KEY.
           03  QLSL1-WEND-BRNO             PIC 9(04).
           03  QLSL1-WEND-ACCTNO           PIC 9(08).
           03  QLSL1-WEND-SEQNO            PIC 9(02).

       01  QLSL-REC.
           03  QLSL-BRNO              PIC 9(4).
           03  QLSL-ACCTNO            PIC 9(8).
           03  QLSL-SEQNO             PIC 99.
           03  QLSL-LTOUCH-DATE           PIC X(10).
           03  QLSL-DATE-SUIT-AUTH        PIC X(10).
           03  QLSL-DATE-SUIT-AUTH-BY     PIC X(20).
           03  QLSL-ATTORNEY-NET-ASSIGN   PIC X(20).
           03  QLSL-LOCAL-ATTORNEY        PIC X(20).
           03  QLSL-ATTORNEY-ADDRESS      PIC X(60).
           03  QLSL-ATTORNEY-PHONE        PIC 9(10).
           03  QLSL-DATE-ASSIGNED         PIC X(10).
           03  QLSL-ATTORNEY-ACCTNO       PIC X(20).
           03  QLSL-PRIN-BAL-SENT         PIC S9(7)V99.
           03  QLSL-RECEIVED-ACKNOW-DATE       PIC X(10).
           03  QLSL-ATTORNEY-LETTER-SENT-DATE  PIC X(10).
           03  QLSL-HOLD-DATE                  PIC X(10).
           03  QLSL-RESPONSE-RECEIVED-DATE     PIC X(10).
           03  QLSL-SUIT-DATE             PIC X(10).
           03  QLSL-SUIT-FILED-DATE       PIC X(10).
           03  QLSL-SERVICE-DATE          PIC X(10).
           03  QLSL-CUSTOMER-SERVED       PIC X.
           03  QLSL-ANSWER-FILED-DATE     PIC X(10).
           03  QLSL-COURT-DATE            PIC X(10).
           03  QLSL-JUDGMENT-TYPE         PIC X(20).
           03  QLSL-JUDGMENT-OBTAINED-DATE PIC X(10).
           03  QLSL-JUDGMENT-COPY-FILED   PIC X.
           03  QLSL-JUDGMENT-AMT          PIC S9(7)V99.
           03  QLSL-JUDGMENT-PRIN         PIC S9(7)V99.
           03  QLSL-POST-JUDG-INT-RATE    PIC S9(3)V99.
           03  QLSL-JUDG-INT-AMT          PIC S9(7)V99.
           03  QLSL-ATTORNEY-FEES-ALLOWED PIC S9(7)V99.
           03  QLSL-OTHER-FEES-ALLOWED    PIC S9(7)V99.
           03  QLSL-JUDG-INT-RATE         PIC S9(3)V99.
           03  QLSL-ASSET-USED-IN-SUIT    PIC X(20).
           03  QLSL-GARN-FILED-DATE       PIC X(10).
           03  QLSL-GARN-TERM-DAYS        PIC 9(3).
           03  QLSL-INTERROGATORY-SENT-DATE PIC X(10).
           03  QLSL-INTERROGATORY-ANSW-DATE PIC X(10).
           03  QLSL-FIFA-SENT-DATE        PIC X(10).
           03  QLSL-FIFA-ANSWERED-DATE    PIC X(10).
           03  QLSL-DATE-TO-RECV-PAY      PIC X(10).
           03  QLSL-FOLLOWUP-DATE         PIC X(10).
           03  QLSL-FIFA-RENEWED-DATE     PIC X(10).
           03  QLSL-PAYS-RECV-ATTORNEY    PIC X.
           03  QLSL-DATE-CLOSED           PIC X(10).
           03  QLSL-REASON-CLOSED         PIC X(20).

       EXEC SQL END DECLARE SECTION END-EXEC.
