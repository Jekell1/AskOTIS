      * COPYMEMBER: LIBLP/LP01LSP_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN STATUS -  PAID THRU (DEFERMENT)
      *
      *=================================================================
      * REV:
      * AMM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LSP1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LSP1-CURSOR-STAT-GOOD             VALUE "00".
           88  LSP1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LSP1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLSP1-WHERE-BEG-KEY.
           03  QLSP1-WBEG-BRNO             PIC 9(04).
           03  QLSP1-WBEG-ACCTNO           PIC 9(08).
           03  QLSP1-WBEG-SEQNO            PIC 9(02).

       01  QLSP1-WHERE-END-KEY.
           03  QLSP1-WEND-BRNO             PIC 9(04).
           03  QLSP1-WEND-ACCTNO           PIC 9(08).
           03  QLSP1-WEND-SEQNO            PIC 9(02).

       01  QLSP-REC.
           03  QLSP-BRNO               PIC 9(4).
           03  QLSP-ACCTNO             PIC 9(8).
           03  QLSP-SEQNO              PIC 99.
           03  QLSP-LTOUCH-DATE        PIC X(10).
           03  QLSP-REASON-CODE           PIC XX.
           03  QLSP-PERIODS-TO-DEFER      PIC 99.
           03  QLSP-REASON-FOR-DEFER      PIC X(50).
           03  QLSP-MORTGAGE-PAST-DUE     PIC X.
           03  QLSP-MORTGAGE-BALANCE      PIC S9(7)V99.
           03  QLSP-ATTEMPT-REFINANCE     PIC X(50).
           03  QLSP-INFO-DATE             PIC X(10).
           03  QLSP-INFO-USERID           PIC X(10).
           03  QLSP-BR-MANAGER-COMMENTS   PIC X(50).
           03  QLSP-BR-MANAGER-DATE       PIC X(10).
           03  QLSP-BR-MANAGER-USERID     PIC X(10).
           03  QLSP-AVP-COMMENTS          PIC X(50).
           03  QLSP-AVP-DATE              PIC X(10).
           03  QLSP-AVP-USERID            PIC X(10).
           03  QLSP-AVP-APPROVE           PIC X.
           03  QLSP-VP-COMMENTS           PIC X(50).
           03  QLSP-VP-DATE               PIC X(10).
           03  QLSP-VP-USERID             PIC X(10).
           03  QLSP-VP-APPROVE            PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
