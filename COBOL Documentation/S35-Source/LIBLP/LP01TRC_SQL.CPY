      * COPYMEMBER: LIBLP/LP01TRC_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE - CHECK/CASH
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRC1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRC1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRC1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRC2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRC2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRC2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRC1-CURSOR-SC-STAT          PIC X(2) VALUE SPACES.
           88  TRC1-CURSOR-SC-STAT-GOOD          VALUE "00".
           88  TRC1-CURSOR-SC-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRC1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  TRC2-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QTRC1-WHERE-BEG-KEY.
           03  QTRC1-WBEG-BRNO             PIC 9(04).
           03  QTRC1-WBEG-KEY-DATE         PIC X(10).
           03  QTRC1-WBEG-CKNO             PIC 9(06).
           03  QTRC1-WBEG-CLASS            PIC 9(03).
           03  QTRC1-WBEG-SEQ              PIC 9(05).

       01  QTRC1-WHERE-END-KEY.
           03  QTRC1-WEND-BRNO             PIC 9(04).
           03  QTRC1-WEND-KEY-DATE         PIC X(10).
           03  QTRC1-WEND-CKNO             PIC 9(06).
           03  QTRC1-WEND-CLASS            PIC 9(03).
           03  QTRC1-WEND-SEQ              PIC 9(05).

       01  QTRC1-WHERE-SCAN-BEG-KEY.
           03  QTRC1-WSBEG-KEY-DATE        PIC X(10).
           03  QTRC1-WSBEG-CKNO            PIC 9(06).
           03  QTRC1-WSBEG-CLASS           PIC 9(03).

       01  QTRC2-WHERE-BEG-KEY.
           03  QTRC2-WBEG-KEY-DATE         PIC X(10).
           03  QTRC2-WBEG-BRNO             PIC 9(04).
           03  QTRC2-WBEG-CKNO             PIC 9(06).
           03  QTRC2-WBEG-CLASS            PIC 9(03).
           03  QTRC2-WBEG-SEQ              PIC 9(05).

       01  QTRC2-WHERE-END-KEY.
           03  QTRC2-WEND-KEY-DATE         PIC X(10).
           03  QTRC2-WEND-BRNO             PIC 9(04).
           03  QTRC2-WEND-CKNO             PIC 9(06).
           03  QTRC2-WEND-CLASS            PIC 9(03).
           03  QTRC2-WEND-SEQ              PIC 9(05).

       01  QTRC-REC.
           03  QTC-BRNO             PIC 9(4).
           03  QTC-KEY-DATE         PIC X(10).
           03  QTC-CKNO             PIC 9(6).
           03  QTC-CLASS            PIC 9(3).
           03  QTC-SEQ              PIC 9(5).
           03  QTC-LTOUCH-DATE      PIC X(10).
           03  QTC-TYPE             PIC X(2).
           03  QTC-DATE             PIC X(10).
           03  QTC-AMT              PIC S9(7)V99.
           03  QTC-PAYEE            PIC X(60).
           03  QTC-REF              PIC 9(6).
           03  QTC-REASON           PIC X(30).
           03  QTC-GLNO-1           PIC 9(11).
           03  QTC-GLAMT-1          PIC S9(7)V99.
           03  QTC-GLNO-2           PIC 9(11).
           03  QTC-GLAMT-2          PIC S9(7)V99.
           03  QTC-GLNO-3           PIC 9(11).
           03  QTC-GLAMT-3          PIC S9(7)V99.
           03  QTC-GLNO-4           PIC 9(11).
           03  QTC-GLAMT-4          PIC S9(7)V99.
           03  QTC-GLNO-5           PIC 9(11).
           03  QTC-GLAMT-5          PIC S9(7)V99.
           03  QTC-VOID             PIC X.
           03  QTC-ACCTNO           PIC 9(8).
           03  QTC-PAID             PIC X.
           03  QTC-PGM              PIC XX.
           03  QTC-DLRVNO           PIC 9.
           03  QTC-SPRCLASS         PIC 999.
           03  QTC-DLNO             PIC 9(4)V99.
           03  QTC-USERID           PIC X(10).
           03  QTC-POSTDATE         PIC X(10).
           03  QTC-POSTTIME         PIC 9(6).
           03  QTC-TILLNO           PIC 9(2).
           03  QTC-ADR1             PIC X(30).
           03  QTC-ADR2             PIC X(30).
           03  QTC-CITY             PIC X(30).
           03  QTC-STATE            PIC X(2).
           03  QTC-ZIP              PIC X(10).
           03  QTC-PAYEE-NO         PIC 9(6).
           03  QTC-ACH-FG           PIC X.
           03  QTC-EXTRACT-FG       PIC X.
           03  QTC-DEBIT-REFCD      PIC X(5).

       EXEC SQL END DECLARE SECTION END-EXEC.
