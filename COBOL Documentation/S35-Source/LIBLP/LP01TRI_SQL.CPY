      * COPYMEMBER: LIBLP/LP01TRI_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE - INSURANCE CLAIMS
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRI1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRI1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRI1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRI2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRI2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRI2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRI1-CURSOR-SC-STAT          PIC X(2) VALUE SPACES.
           88  TRI1-CURSOR-SC-STAT-GOOD          VALUE "00".
           88  TRI1-CURSOR-SC-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRI1-RETURN-CODE                 PIC S9(4)   COMP-5.
       01  TRI2-RETURN-CODE                 PIC S9(4)   COMP-5.

       01  QTRI1-WHERE-BEG-KEY.
           03  QTRI1-WBEG-BRNO             PIC 9(04).
           03  QTRI1-WBEG-DATE             PIC X(10).
           03  QTRI1-WBEG-SEQ              PIC 9(05).
       01  QTRI1-WHERE-END-KEY.
           03  QTRI1-WEND-BRNO             PIC 9(04).
           03  QTRI1-WEND-DATE             PIC X(10).
           03  QTRI1-WEND-SEQ              PIC 9(05).

       01  QTRI1-WHERE-SCAN-BEG-KEY.
           03  QTRI1-WSBEG-DATE            PIC X(10).

       01  QTRI2-WHERE-BEG-KEY.
           03  QTRI2-WBEG-DATE             PIC X(10).
           03  QTRI2-WBEG-BRNO             PIC 9(04).
           03  QTRI2-WBEG-SEQ              PIC 9(05).
       01  QTRI2-WHERE-END-KEY.
           03  QTRI2-WEND-DATE             PIC X(10).
           03  QTRI2-WEND-BRNO             PIC 9(04).
           03  QTRI2-WEND-SEQ              PIC 9(05).
 
       01  QTRI-REC.
           03  QTI-BRNO             PIC 9(4).
           03  QTI-KEY-DATE         PIC X(10).
           03  QTI-SEQ              PIC 9(5).
           03  QTI-LTOUCH-DATE      PIC X(10).
           03  QTI-WAIS                 PIC X(4).
           03  QTI-FILLER               PIC X(4).
           03  QTI-TYPE                 PIC 9(3).
           03  QTI-AMOUNT               PIC S9(8)V99.
           03  QTI-CLAIM-NUMBER         PIC X(9).
           03  QTI-FILLER2              PIC X(13).
           03  QTI-LNAME                PIC X(15).
           03  QTI-FNAME                PIC X(10).
           03  QTI-DATE                 PIC X(10).
           03  QTI-DRAFT                PIC 9(8).
           03  QTI-FILLER3              PIC X(40).
           03  QTI-PRODUCER             PIC 9(6).
           03  QTI-USERID               PIC X(8).
           03  QTI-POSTTIME             PIC 9(6).
           03  QTI-POSTDATE             PIC X(10).
           03  QTI-INSCOMP              PIC 9(2).
           03  QTI-DAYS-PAID            PIC 9(6).
           03  QTI-PMT-TYPE             PIC X(10).
           03  QTI-FROM-DATE            PIC X(10).
           03  QTI-TO-DATE              PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
