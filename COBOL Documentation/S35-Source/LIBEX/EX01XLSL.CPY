      * COPYMEMBER: LIBEX/EX01XLSL
      ******************************************************************
      *    LOAN LEGAL STATUS INFORMATION
      *
      * REV:
      *  160503 BAH NEW FOR SOAUTO PR#5064
      ******************************************************************
       78  XLSL-SIZE                     VALUE 585.
       01  XLSL-REC.
           03  XLS-L-BRACCTNO             PIC 9(10).
           03  XLS-L-ACCTNO               PIC 9(8).
           03  XLS-L-CODE                 PIC X.
           03  XLS-L-SEQNO                PIC 9.
           
           03  XLS-L-DATE-SUIT-AUTH        PIC 9(8).
           03  XLS-L-DATE-SUIT-AUTH-BY     PIC X(20).
           03  XLS-L-ATTORNEY-NET-ASSIGN   PIC X(20).
           03  XLS-L-LOCAL-ATTORNEY        PIC X(20).
           03  XLS-L-ATTORNEY-ADDRESS      PIC X(60).
           03  XLS-L-ATTORNEY-PHONE        PIC 9(10).
           03  XLS-L-DATE-ASSIGNED         PIC 9(8).
           03  XLS-L-ATTORNEY-ACCTNO       PIC X(20).
           03  XLS-L-PRIN-BAL-SENT         PIC +9(6).99.
           03  XLS-L-RECEIVED-ACKNOW       PIC 9(8).
           03  XLS-L-ATTORNEY-LETTER-SENT  PIC 9(8).
           03  XLS-L-HOLD-DATE             PIC 9(8).
           03  XLS-L-RESPONSE-RECEIVED     PIC 9(8).
           03  XLS-L-SUIT-DATE             PIC 9(8).
           03  XLS-L-SUIT-FILED            PIC 9(8).
           03  XLS-L-SERVICE-DATE          PIC 9(8).
           03  XLS-L-CUSTOMER-SERVED       PIC X.
           03  XLS-L-ANSWER-FILED          PIC 9(8).
           03  XLS-L-COURT-DATE            PIC 9(8).
           03  XLS-L-JUDGMENT-TYPE         PIC X(20).
           03  XLS-L-JUDGMENT-OBTAINED     PIC 9(8).
           03  XLS-L-JUDGMENT-COPY-FILED   PIC X.
           03  XLS-L-JUDGMENT-AMT          PIC +9(7).99.
           03  XLS-L-JUDGMENT-PRIN         PIC +9(7).99.
           03  XLS-L-POST-JUDG-INT-RATE    PIC +9(3).99.
           03  XLS-L-JUDG-INT-AMT          PIC +9(7).99.
           03  XLS-L-ATTORNEY-FEES-ALLOWED PIC +9(7).99.
           03  XLS-L-OTHER-FEES-ALLOWED    PIC +9(7).99.
           03  XLS-L-JUDG-INT-RATE         PIC +9(3).99.
           03  XLS-L-ASSET-USED-IN-SUIT    PIC X(20).
           03  XLS-L-GARN-FILED-DATE       PIC 9(8).
           03  XLS-L-GARN-TERM-DAYS        PIC 9(3).
           03  XLS-L-INTERROGATORY-SENT    PIC 9(8).
           03  XLS-L-INTERROGATORY-ANSW    PIC 9(8).
           03  XLS-L-FIFA-SENT             PIC 9(8).
           03  XLS-L-FIFA-ANSWERED         PIC 9(8).
           03  XLS-L-DATE-TO-RECV-PAY      PIC 9(8).
           03  XLS-L-FOLLOWUP-DATE         PIC 9(8).
           03  XLS-L-FIFA-RENEWED          PIC 9(8).
           03  XLS-L-PAYS-RECV-ATTORNEY    PIC X.
           03  XLS-L-DATE-CLOSED           PIC 9(8).
           03  XLS-L-REASON-CLOSED         PIC X(20).            
           
           03  FILLER                      PIC X(100).
           03  XLS-L-CR                    PIC X.
           03  XLS-L-LF                    PIC X.

