      * COPYMEMBER: LIBLP/LP01LTB_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN TRAILER BANKRUPCY FILE
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LTB1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LTB1-CURSOR-STAT-GOOD             VALUE "00".
           88  LTB1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LTB1-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QLTB1-WHERE-BEG-KEY.
           03  QLTB1-WBEG-BRNO             PIC 9(04).
           03  QLTB1-WBEG-ACCTNO           PIC 9(08).
           03  QLTB1-WBEG-SEQNO            PIC 9(03).
 
       01  QLTB1-WHERE-END-KEY.
           03  QLTB1-WEND-BRNO             PIC 9(04).
           03  QLTB1-WEND-ACCTNO           PIC 9(08).
           03  QLTB1-WEND-SEQNO            PIC 9(03).
 
       01  QLTB-REC.
           03  QLTB-BRNO              PIC 9(4).
           03  QLTB-ACCTNO            PIC 9(8).
           03  QLTB-SEQNO             PIC 999.
           03  QLTB-LTOUCH-DATE       PIC X(10).
           03  QLTB-FILEDATE         PIC X(10).
           03  QLTB-CASE-NO          PIC X(9).     
           03  QLTB-CHAPTER          PIC 99.
           03  QLTB-MISC1            PIC X(30).
           03  QLTB-MISC2            PIC X(30).
           03  QLTB-MISC3            PIC X(30).
           03  QLTB-BDDATE           PIC X(10).
           03  QLTB-WHO-FILED        PIC X.
           03  QLTB-ENDORSER-FILED   PIC X.
           03  QLTB-CLAIM-AMT        PIC S9(7)V99.
           03  QLTB-ACTIVE-INACTIVE  PIC X.
           03  QLTB-INT-LC-DUE       PIC S9(7)V99.
           03  QLTB-OTH-DUE          PIC S9(7)V99.
           03  QLTB-TDUEAMT          PIC S9(7)V99.
           03  QLTB-NAME-1             PIC X(24).
           03  QLTB-ADR1-1             PIC X(24).
           03  QLTB-ADR2-1             PIC X(24).
           03  QLTB-CITY-1             PIC X(15).
           03  QLTB-STATE-1            PIC XX.
           03  QLTB-ZIP-1              PIC X(10).
           03  QLTB-PHONE-1            PIC 9(10).
           03  QLTB-NAME-2             PIC X(24).
           03  QLTB-ADR1-2             PIC X(24).
           03  QLTB-ADR2-2             PIC X(24).
           03  QLTB-CITY-2             PIC X(15).
           03  QLTB-STATE-2            PIC XX.
           03  QLTB-ZIP-2              PIC X(10).
           03  QLTB-PHONE-2            PIC 9(10).
           03  QLTB-NAME-3             PIC X(24).
           03  QLTB-ADR1-3             PIC X(24).
           03  QLTB-ADR2-3             PIC X(24).
           03  QLTB-CITY-3             PIC X(15).
           03  QLTB-STATE-3            PIC XX.
           03  QLTB-ZIP-3              PIC X(10).
           03  QLTB-PHONE-3            PIC 9(10).
           03  QLTB-REAFF-DATE           PIC X(10).
           03  QLTB-REAFF-SYSDATE        PIC X(10).
           03  QLTB-REAFF-BALANCE        PIC 9(6)V99.
           03  QLTB-REAFF-PAYAMT         PIC 9(6)V99.
           03  QLTB-REAFF-NO-PAYS        PIC 9(3).
           03  QLTB-INTRATE              PIC 9(3)V9(3).
           03  QLTB-RESCIND-DATE         PIC X(10).
           03  QLTB-PARTIAL-FULL         PIC X.
           03  QLTB-DISCHARGE-DATE       PIC X(10).
           03  QLTB-DISCHARGE-SYSDATE    PIC X(10).
           03  QLTB-GROSS-BAL            PIC S9(7)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
