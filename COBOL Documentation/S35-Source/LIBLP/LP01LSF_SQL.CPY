      * COPYMEMBER: LIBLP/LP01LSF_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN STATUS -  FOLLOW UP
      *
      *=================================================================
      * REV:
      * AMM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LSF1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LSF1-CURSOR-STAT-GOOD             VALUE "00".
           88  LSF1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LSF1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLSF1-WHERE-BEG-KEY.
           03  QLSF1-WBEG-BRNO             PIC 9(04).
           03  QLSF1-WBEG-ACCTNO           PIC 9(08).
           03  QLSF1-WBEG-SEQNO            PIC 9(02).

       01  QLSF1-WHERE-END-KEY.
           03  QLSF1-WEND-BRNO             PIC 9(04).
           03  QLSF1-WEND-ACCTNO           PIC 9(08).
           03  QLSF1-WEND-SEQNO            PIC 9(02).

       01  QLSF-REC.
           03  QLSF-BRNO               PIC 9(4).
           03  QLSF-ACCTNO             PIC 9(8).
           03  QLSF-SEQNO              PIC 99.
           
           03  QLSF-LTOUCH-DATE            PIC X(10).
           03  QLSF-SKIP-START-DATE        PIC X(10).
           03  QLSF-SKIP-END-DATE          PIC X(10).
           03  QLSF-OFR-START-DATE         PIC X(10).
           03  QLSF-OFR-END-DATE           PIC X(10).
           03  QLSF-IMPOUND-START-DATE     PIC X(10).
           03  QLSF-IMPOUND-END-DATE       PIC X(10).
           03  QLSF-BNKRPT-START-DATE      PIC X(10).
           03  QLSF-BNKRPT-END-DATE        PIC X(10).
           03  QLSF-INSUR-START-DATE       PIC X(10).
           03  QLSF-INSUR-END-DATE         PIC X(10).
           03  QLSF-COLLECT-START-DATE     PIC X(10).
           03  QLSF-COLLECT-END-DATE       PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
