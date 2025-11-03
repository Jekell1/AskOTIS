      * COPYMEMBER: LIBLP/LP01LTL_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN TRAILER FILE - LIENHOLDER
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LTL1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LTL1-CURSOR-STAT-GOOD             VALUE "00".
           88  LTL1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LTL1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLTL1-WHERE-BEG-KEY.
           03  QLTL1-WBEG-BRNO             PIC 9(04).
           03  QLTL1-WBEG-ACCTNO           PIC 9(08).
           03  QLTL1-WBEG-SEQNO            PIC 9(03).

       01  QLTL1-WHERE-END-KEY.
           03  QLTL1-WEND-BRNO             PIC 9(04).
           03  QLTL1-WEND-ACCTNO           PIC 9(08).
           03  QLTL1-WEND-SEQNO            PIC 9(03).

       01  QLTL-REC.
           03  QLTL-BRNO               PIC 9(4).
           03  QLTL-ACCTNO             PIC 9(8).
           03  QLTL-SEQNO              PIC 999.
           03  QLTL-LTOUCH-DATE        PIC X(10).
           03  QLTL-LIEN-TYPE-1      PIC 9.
           03  QLTL-LIEN-NAME-1      PIC X(30).
           03  QLTL-LIEN-ADR-1       PIC X(24).
           03  QLTL-LIEN-CITY-1      PIC X(15).
           03  QLTL-LIEN-STATE-1     PIC X(2).
           03  QLTL-LIEN-ZIP-1       PIC X(10).
           03  QLTL-LIEN-DATE-1      PIC X(10).
           03  QLTL-LIEN-TYPE-2      PIC 9.
           03  QLTL-LIEN-NAME-2      PIC X(30).
           03  QLTL-LIEN-ADR-2       PIC X(24).
           03  QLTL-LIEN-CITY-2      PIC X(15).
           03  QLTL-LIEN-STATE-2     PIC X(2).
           03  QLTL-LIEN-ZIP-2       PIC X(10).
           03  QLTL-LIEN-DATE-2      PIC X(10).
           03  QLTL-LIEN-TYPE-3      PIC 9.
           03  QLTL-LIEN-NAME-3      PIC X(30).
           03  QLTL-LIEN-ADR-3       PIC X(24).
           03  QLTL-LIEN-CITY-3      PIC X(15).
           03  QLTL-LIEN-STATE-3     PIC X(2).
           03  QLTL-LIEN-ZIP-3       PIC X(10).
           03  QLTL-LIEN-DATE-3      PIC X(10).
           03  QLTL-TITLE-RCV-DATE     PIC X(10).
           03  QLTL-TITLE-LIEN-DATE    PIC X(10).
           03  QLTL-TITLE-NO           PIC X(20).
           03  QLTL-LIEN-LETTER-SENT   PIC X.
           03  QLTL-LIEN-COMMENT1      PIC X(60).
           03  QLTL-LIEN-COMMENT2      PIC X(60).
           03  QLTL-LIEN-TITLE-STATE   PIC XX.
           03  QLTL-TITLE-STATUS-CODE  PIC XX.
           03  QLTL-TITLE-SEND-FLAG    PIC X.
           03  QLTL-TITLE-NAME         PIC X(25).
           03  QLTL-TITLE-ADR          PIC X(25).
           03  QLTL-TITLE-CITY         PIC X(15).
           03  QLTL-TITLE-STATE        PIC X(2).
           03  QLTL-TITLE-ZIP          PIC X(10).
           03  QLTL-TITLE-DATE         PIC X(10).
           03  QLTL-TITLE-SENT-BY      PIC X(25).
           03  QLTL-SEND-ASAP          PIC X.
           03  QLTL-TITLE-SEND-FMNO    PIC 9(4).
           03  QLTL-TITLE-ADR2         PIC X(22).
           03  QLTL-TITLE-COMMENT1     PIC X(40).
           03  QLTL-TITLE-COMMENT2     PIC X(40).
           03  QLTL-RELEASE-TYPE       PIC XX.
           03  QLTL-CARRIER-TYPE       PIC X(4).
           03  QLTL-CARRIER-ACCTNO     PIC X(20).

       EXEC SQL END DECLARE SECTION END-EXEC.
